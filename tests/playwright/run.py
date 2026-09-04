#!/usr/bin/env python3
# Halemans Playwright E2E suite (design_docs/milestone_1.md §10).
# Runs against the stack booted by smoke-check.sh; reads the same state dir.
import json
import os
import subprocess
import sys
import time
import urllib.request

os.environ.setdefault("DEBUG", "pw:browser")  # surface chromium stderr in check logs

from playwright.sync_api import sync_playwright

APP = os.environ.get("HALEMANS_APP_URL", "http://127.0.0.1:28080")
STATE = os.environ["DEVENV_STATE"]
DATABASE_URL = os.environ["DATABASE_URL"]

failures = []


def check(name):
    def deco(fn):
        try:
            fn()
            print(f"  PASS {name}", flush=True)
        except Exception as e:
            print(f"  FAIL {name}: {type(e).__name__} {e}", flush=True)
            failures.append(name)
    return deco


def sql(query):
    return subprocess.run(
        ["psql", DATABASE_URL, "-tA", "-c", query],
        check=True, capture_output=True, text=True,
    ).stdout.strip()


def password(role):
    with open(os.path.join(STATE, "halemans", f"{role}-password")) as f:
        return f.read().strip()


def fire_generic_alert(fingerprint, severity="warning", status="firing", title="playwright test alert"):
    token = open(os.path.join(STATE, "halemans", "generic-hook-token")).read().strip()
    payload = {
        "version": "4", "status": status, "receiver": "halemans",
        "alerts": [{
            "status": status,
            "labels": {"alertname": "pw-test", "env": "dev", "host": "dev-host-01",
                       "severity": severity, "check": "pw-test"},
            "annotations": {"summary": title},
            "startsAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "fingerprint": fingerprint,
        }],
    }
    req = urllib.request.Request(
        f"{APP}/hooks/generic/{token}", data=json.dumps(payload).encode(),
        headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req) as resp:
        assert resp.status == 200, resp.status


def login(page, role):
    page.goto(f"{APP}/NewSession")
    page.get_by_test_id("login-email").fill(f"{role}@dev")
    page.get_by_test_id("login-password").fill(password(role))
    page.get_by_test_id("login-submit").click()
    page.wait_for_url(APP + "/")


with sync_playwright() as pw:
    browser = pw.chromium.launch(args=[
        "--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu",
        "--no-zygote", "--disable-crash-reporter", "--disable-crashpad",
    ])
    browser.on("disconnected", lambda _: print("  [browser disconnected]", flush=True))
    context = browser.new_context()

    @check("login as each role lands on the dashboard")
    def _():
        for role in ["admin", "sre", "viewer"]:
            page = context.new_page()
            login(page, role)
            page.get_by_test_id("env-cards").wait_for()
            page.close()

    page = context.new_page()
    login(page, "sre")

    @check("dashboard shows the dev environment card with counts")
    def _():
        page.goto(APP + "/")
        page.get_by_test_id("env-cards").wait_for()
        card = page.get_by_test_id("env-card").filter(has_text="dev").first
        card.wait_for()
        text = card.get_by_test_id("count-firing").inner_text()
        assert text.endswith("firing"), f"unexpected count text: {text!r} in card {card.inner_text()!r}"

    @check("environment page renders alerts and accepts filters")
    def _():
        page.goto(f"{APP}/env/dev")
        page.get_by_test_id("env-alerts-table").wait_for()
        page.select_option("select[name=severity]", "critical")
        page.get_by_role("button", name="Filter").click()
        page.get_by_test_id("env-alerts-table").wait_for()

    @check("alert card: ack with timeout, comment, timeline updates")
    def _():
        fp = f"pw-ack-{int(time.time())}"
        fire_generic_alert(fp)
        deadline = time.time() + 30
        alert_id = None
        while time.time() < deadline:
            alert_id = sql(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'") or None
            if alert_id:
                break
            time.sleep(1)
        assert alert_id, "alert never arrived"
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("alert-card").wait_for()
        page.get_by_test_id("ack-timeout-button").click()
        page.get_by_test_id("alert-status").filter(has_text="ack").wait_for()
        page.get_by_test_id("comment-body").fill("investigating")
        page.get_by_test_id("comment-submit").click()
        page.get_by_test_id("alert-comments").get_by_text("investigating").wait_for()
        kinds = page.get_by_test_id("alert-timeline").inner_text()
        assert "created" in kinds and "ack" in kinds and "comment" in kinds

    @check("websocket: server-side alert lands in the DOM without reload")
    def _():
        page.goto(f"{APP}/alerts")
        page.get_by_test_id("alerts-table").wait_for()
        fp = f"pw-ws-{int(time.time())}"
        fire_generic_alert(fp, title="ws live update alert")
        row = page.locator(f'tr[data-fingerprint="grafana:{fp}"]')
        row.wait_for(timeout=15000)

    @check("websocket: alert card status badge updates live on resolve")
    def _():
        fp = f"pw-wsres-{int(time.time())}"
        fire_generic_alert(fp)
        deadline = time.time() + 30
        alert_id = None
        while time.time() < deadline:
            alert_id = sql(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'") or None
            if alert_id:
                break
            time.sleep(1)
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("alert-status").filter(has_text="firing").wait_for()
        fire_generic_alert(fp, status="resolved")
        page.get_by_test_id("alert-status").filter(has_text="resolved").wait_for(timeout=15000)

    @check("blackout: created via UI, new alerts arrive muted")
    def _():
        admin = context.new_page()
        login(admin, "admin")  # manage_blackouts privilege required
        admin.goto(f"{APP}/blackouts")
        page = admin
        page.get_by_test_id("new-blackout").click()
        start = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() - 60))
        end = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() + 3600))
        page.get_by_test_id("blackout-starts-at").fill(start)
        page.get_by_test_id("blackout-ends-at").fill(end)
        page.get_by_test_id("blackout-reason").fill("playwright window")
        page.get_by_test_id("blackout-submit").click()
        page.get_by_test_id("blackout-row").first.wait_for()
        fp = f"pw-bo-{int(time.time())}"
        fire_generic_alert(fp)
        deadline = time.time() + 30
        while time.time() < deadline:
            if sql(f"SELECT 1 FROM alerts WHERE fingerprint = 'grafana:{fp}' AND suppressed"):
                break
            time.sleep(1)
        else:
            raise AssertionError("alert not suppressed by blackout")
        page.goto(f"{APP}/env/dev")
        row = page.locator(f'tr[data-fingerprint="grafana:{fp}"]')
        row.wait_for()
        assert "suppressed" in (row.get_attribute("class") or "")
        # cleanup: end the blackout so later tests are unaffected
        sql("UPDATE blackouts SET ends_at = now() - interval '1 min' WHERE reason = 'playwright window'")
        admin.close()
        page = context.new_page()
        login(page, "sre")

    @check("push subscribe endpoint accepts a subscription")
    def _():
        result = page.evaluate("""async () => {
            const body = JSON.stringify({
                endpoint: "https://push.example.com/sub/playwright-test",
                keys: { p256dh: "BP7Xf0", auth: "dGVzdA" }
            });
            const res = await fetch('/api/push/subscribe', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body });
            return res.status;
        }""")
        assert result == 200, result

    @check("viewer gets 403 on ack")
    def _():
        viewer = context.new_page()
        login(viewer, "viewer")
        firing = sql("SELECT id FROM alerts WHERE status = 'firing' LIMIT 1")
        assert firing, "no firing alert to probe"
        result = viewer.evaluate(f"""async () => {{
            const res = await fetch('/alerts/{firing}/ack', {{ method: 'POST' }});
            return res.status;
        }}""")
        assert result == 403, result
        viewer.close()

    browser.close()

print()
if failures:
    print(f"playwright: {len(failures)} failure(s)")
    sys.exit(1)
print("playwright: all scenarios passed")
