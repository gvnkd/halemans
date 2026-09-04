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


def fire_generic_alert(fingerprint, severity="warning", status="firing", title="playwright test alert", host="dev-host-01"):
    token = open(os.path.join(STATE, "halemans", "generic-hook-token")).read().strip()
    payload = {
        "version": "4", "status": status, "receiver": "halemans",
        "alerts": [{
            "status": status,
            "labels": {"alertname": "pw-test", "env": "dev", "host": host,
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


def wait_sql_value(query, timeout=30):
    deadline = time.time() + timeout
    while time.time() < deadline:
        value = sql(query)
        if value:
            return value
        time.sleep(1)
    return None


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

    @check("viewer gets 403 on all admin pages")
    def _():
        viewer = context.new_page()
        login(viewer, "viewer")
        for path in ["/admin/teams", "/admin/grouping-rules", "/admin/notification-rules",
                     "/admin/escalation-policies", "/sources/new"]:
            result = viewer.evaluate(f"""async () => {{
                const res = await fetch('{path}');
                return res.status;
            }}""")
            assert result == 403, f"{path}: {result}"
        viewer.close()

    # ---------------------------------------------------------- milestone 2

    @check("grouping: rule via admin UI, two alerts roll into one group, group card + group ack")
    def _():
        ts = int(time.time())
        host = f"pw-group-host-{ts}"
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin/grouping-rules")
        admin.get_by_test_id("new-grouping-rule").click()
        admin.get_by_test_id("rule-name").fill("pw-rule")
        admin.get_by_test_id("rule-position").fill("-1")
        admin.get_by_test_id("rule-template").fill("{host}-pw")
        admin.get_by_test_id("grouping-rule-submit").click()
        admin.get_by_test_id("grouping-rules-table").get_by_text("pw-rule").wait_for()
        # rule preview renders
        admin.get_by_test_id("grouping-rule-row").filter(has_text="pw-rule").get_by_test_id("preview-grouping-rule").click()
        admin.get_by_test_id("grouping-rule-preview-table").wait_for()

        fire_generic_alert(f"pw-grp-a-{ts}", title=f"pw group member A {ts}", host=host)
        fire_generic_alert(f"pw-grp-b-{ts}", title=f"pw group member B {ts}", host=host)
        group_id = wait_sql_value(
            f"SELECT id FROM alert_groups WHERE group_key = '{host}-pw' AND member_count = 2", 60)
        assert group_id, "group with 2 members never appeared"

        admin.goto(f"{APP}/groups/{group_id}")
        admin.get_by_test_id("group-header").wait_for()
        body = admin.content()
        assert f"pw group member A {ts}" in body and f"pw group member B {ts}" in body

        # env page grouped view rolls up the group
        admin.goto(f"{APP}/env/dev?view=grouped")
        admin.get_by_test_id("env-groups-table").wait_for()
        admin.locator(f'tr[data-group-key="{host}-pw"]').wait_for()

        # group ack acts on all firing members
        admin.goto(f"{APP}/groups/{group_id}")
        admin.get_by_test_id("ack-group").click()
        deadline = time.time() + 30
        while time.time() < deadline:
            unacked = sql(f"SELECT count(*) FROM alerts WHERE group_id = '{group_id}' AND status = 'firing'")
            if unacked == "0":
                break
            time.sleep(1)
        assert sql(f"SELECT count(*) FROM alerts WHERE group_id = '{group_id}' AND status = 'firing'") == "0"
        # cleanup so later severity-threshold rules don't see stale state
        sql(f"UPDATE grouping_rules SET enabled = false WHERE name = 'pw-rule'")
        admin.close()

    @check("grouping rule edit bumps version")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        version_before = sql("SELECT version FROM grouping_rules WHERE name = 'pw-rule'")
        admin.goto(f"{APP}/admin/grouping-rules")
        admin.get_by_test_id("grouping-rule-row").filter(has_text="pw-rule").get_by_test_id("edit-grouping-rule").click()
        admin.get_by_test_id("rule-template").fill("{host}-pw-v2")
        admin.get_by_test_id("grouping-rule-submit").click()
        admin.get_by_test_id("grouping-rules-table").wait_for()
        version_after = sql("SELECT version FROM grouping_rules WHERE name = 'pw-rule'")
        assert int(version_after) == int(version_before) + 1, (version_before, version_after)
        admin.close()

    @check("blackout edit changes the window without recreate")
    def _():
        ts = int(time.time())
        host = f"pw-boedit-host-{ts}"
        fire_generic_alert(f"pw-boedit-seed-{ts}", title=f"pw boedit seed {ts}", host=host)
        assert wait_sql_value(f"SELECT id FROM hosts WHERE fqdn = '{host}'"), "host never auto-created"
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/blackouts")
        admin.get_by_test_id("new-blackout").click()
        admin.select_option("[data-testid=blackout-scope-type]", "host")
        admin.select_option("[data-testid=blackout-scope-id]", label=host)
        start = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() - 60))
        end = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() + 3600))
        admin.get_by_test_id("blackout-starts-at").fill(start)
        admin.get_by_test_id("blackout-ends-at").fill(end)
        admin.get_by_test_id("blackout-reason").fill("pw edit me")
        admin.get_by_test_id("blackout-submit").click()
        row = admin.get_by_test_id("blackout-row").filter(has_text="pw edit me")
        row.wait_for()
        blackout_id = sql("SELECT id FROM blackouts WHERE reason = 'pw edit me'")
        row.get_by_test_id("edit-blackout").click()
        new_end = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() + 7200))
        admin.get_by_test_id("blackout-ends-at").fill(new_end)
        admin.get_by_test_id("blackout-reason").fill("pw edited")
        admin.get_by_test_id("blackout-submit").click()
        admin.get_by_test_id("blackout-row").filter(has_text="pw edited").wait_for()
        same_id = sql("SELECT id FROM blackouts WHERE reason = 'pw edited'")
        assert same_id == blackout_id, "edit recreated the row"
        sql("DELETE FROM blackouts WHERE id = '%s'" % blackout_id)  # cleanup
        admin.close()

    @check("sources admin: create, edit, disable, re-enable")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/sources")
        admin.get_by_test_id("new-source").click()
        admin.get_by_test_id("source-name").fill("pw-source")
        admin.select_option("[data-testid=source-type]", "webhook")
        admin.get_by_test_id("source-base-url").fill("http://example.invalid")
        admin.get_by_test_id("source-submit").click()
        row = admin.get_by_test_id("source-row").filter(has_text="pw-source")
        row.wait_for()
        row.get_by_test_id("edit-source").click()
        admin.get_by_test_id("source-name").fill("pw-source-renamed")
        admin.get_by_test_id("source-submit").click()
        row = admin.get_by_test_id("source-row").filter(has_text="pw-source-renamed")
        row.wait_for()
        row.get_by_test_id("toggle-source").click()
        row = admin.get_by_test_id("source-row").filter(has_text="pw-source-renamed")
        row.get_by_text("disabled").wait_for()
        row.get_by_test_id("toggle-source").click()
        admin.get_by_test_id("source-row").filter(has_text="pw-source-renamed").get_by_text("enabled").wait_for()
        sql("DELETE FROM sources WHERE name = 'pw-source-renamed'")  # cleanup
        admin.close()

    @check("notification rule CRUD with escalation policy attach")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin/escalation-policies")
        admin.get_by_test_id("new-escalation-policy").click()
        admin.get_by_test_id("policy-name").fill("pw-policy")
        admin.get_by_test_id("policy-step-0").get_by_test_id("step-after").fill("600")
        admin.get_by_test_id("policy-step-0").get_by_test_id("step-target").select_option(label="team: sre")
        admin.get_by_test_id("policy-submit").click()
        admin.get_by_test_id("escalation-policy-row").filter(has_text="pw-policy").wait_for()

        admin.goto(f"{APP}/admin/notification-rules")
        admin.get_by_test_id("new-notification-rule").click()
        admin.get_by_test_id("rule-name").fill("pw-notify")
        admin.get_by_test_id("rule-severity-threshold").select_option("critical")
        admin.get_by_test_id("rule-target").select_option(label="team: sre")
        admin.get_by_test_id("rule-throttle").fill("60")
        admin.get_by_test_id("rule-escalation-policy").select_option(label="pw-policy")
        admin.get_by_test_id("notification-rule-submit").click()
        row = admin.get_by_test_id("notification-rule-row").filter(has_text="pw-notify")
        row.wait_for()
        assert "team: sre" in row.inner_text()
        # cleanup: rule would otherwise escalate every critical test alert
        sql("DELETE FROM notification_rules WHERE name = 'pw-notify'")
        sql("DELETE FROM escalation_policies WHERE name = 'pw-policy'")
        admin.close()

    @check("teams admin: create team with members, edit, delete")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin/teams")
        admin.get_by_test_id("new-team").click()
        admin.get_by_test_id("team-name").fill("pw-team")
        admin.get_by_test_id("member-sre@dev").select_option("lead")
        admin.get_by_test_id("team-submit").click()
        row = admin.get_by_test_id("team-row").filter(has_text="pw-team")
        row.wait_for()
        row_text = row.inner_text()
        assert "sre@dev (lead)" in row_text, f"members cell: {row_text!r}"
        on_call = sql("""SELECT members->>0 FROM on_call_schedules s
                         JOIN teams t ON t.id = s.team_id WHERE t.name = 'pw-team'""")
        sre_id = sql("SELECT id FROM users WHERE email = 'sre@dev'")
        assert on_call == sre_id, f"on-call {on_call!r} != sre {sre_id!r}"
        row.get_by_test_id("edit-team").click()
        admin.get_by_test_id("team-description").fill("edited")
        admin.get_by_test_id("team-submit").click()
        admin.get_by_test_id("team-row").filter(has_text="edited").wait_for()
        admin.get_by_test_id("team-row").filter(has_text="pw-team").get_by_role("button", name="Delete").click()
        deadline = time.time() + 15
        while time.time() < deadline:
            if not sql("SELECT 1 FROM teams WHERE name = 'pw-team'"):
                break
            time.sleep(1)
        assert not sql("SELECT 1 FROM teams WHERE name = 'pw-team'"), "team not deleted"
        admin.close()

    browser.close()

print()
if failures:
    print(f"playwright: {len(failures)} failure(s)")
    sys.exit(1)
print("playwright: all scenarios passed")
