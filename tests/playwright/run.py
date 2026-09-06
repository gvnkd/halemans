#!/usr/bin/env python3
# Halemans Playwright E2E suite (design_docs/milestone_1.md §10).
# Runs against the stack booted by smoke-check.sh; reads the same state dir.
import json
import os
import re
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


UUID_RE = re.compile(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")


def inserted_id(insert_output):
    # psql prints the command tag ("INSERT 0 1") after the RETURNING value
    return UUID_RE.search(insert_output).group(0)


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
        page.wait_for_load_state("networkidle")
        page.get_by_test_id("env-alerts-table").wait_for()
        page.get_by_test_id("env-filters-reset").click()
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
        if not firing:
            fp = f"pw-rbac-{int(time.time())}"
            fire_generic_alert(fp)
            firing = wait_sql_value(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'", 30)
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
                     "/admin/escalation-policies", "/sources/new", "/admin/llm"]:
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
        admin.get_by_test_id("team-host-groups-empty").wait_for()
        assert not admin.get_by_test_id("member-sre@dev").is_visible()
        admin.get_by_test_id("team-members-filter").fill("sre@dev")
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

    # ---------------------------------------------------------- milestone 3

    @check("cmdb panel renders on alert card and survives manual refresh")
    def _():
        fp = f"pw-cmdb-{int(time.time())}"
        fire_generic_alert(fp)
        alert_id = wait_sql_value(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'", 30)
        assert alert_id, "alert never arrived"
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("cmdb-panel").wait_for()
        page.locator('[data-testid="cmdb-entry"], [data-testid="cmdb-negative"]').first.wait_for(timeout=15000)
        page.get_by_test_id("cmdb-refresh").click()
        page.get_by_test_id("cmdb-panel").wait_for()

    @check("jira: create ticket from alert card links with origin manual")
    def _():
        alert_id = sql("SELECT id FROM alerts WHERE fingerprint LIKE 'grafana:pw-cmdb-%' ORDER BY created_at DESC LIMIT 1")
        assert alert_id, "no alert from cmdb check"
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("jira-create-form").wait_for()
        page.get_by_test_id("jira-summary").fill(f"pw smoke ticket {int(time.time())}")
        page.get_by_test_id("jira-create-submit").click()
        page.get_by_test_id("jira-origin").filter(has_text="manual").first.wait_for()
        link = sql(f"SELECT 1 FROM jira_links WHERE alert_id = '{alert_id}' AND origin = 'manual' LIMIT 1")
        assert link == "1", "manual jira link row missing"

    @check("dashboard CRUD: create, open, set default, move, delete")
    def _():
        sql("DELETE FROM dashboards WHERE name = 'My board'")  # pre-clean from failed runs
        page.goto(f"{APP}/dashboards/new")
        page.get_by_test_id("dashboard-form").wait_for()
        page.get_by_test_id("dashboard-name").fill("My board")
        page.get_by_test_id("dashboard-config").fill('[{"env":"dev","filters":{"status":["firing"],"severity":[]}}]')
        page.get_by_test_id("dashboard-submit").click()
        row = page.get_by_test_id("dashboard-row").filter(has_text="My board")
        row.wait_for()
        dash_id = sql("SELECT id FROM dashboards WHERE name = 'My board'")
        assert dash_id, "dashboard row missing"
        row.get_by_test_id("dashboard-link").click()
        page.get_by_test_id("dashboard-title").wait_for()
        assert "My board" in page.get_by_test_id("dashboard-title").inner_text()
        page.get_by_test_id("dashboard-card-dev").wait_for()

        page.goto(f"{APP}/dashboards")
        row = page.get_by_test_id("dashboard-row").filter(has_text="My board")
        row.get_by_test_id("set-default-dashboard").click()
        row.get_by_test_id("dashboard-default").wait_for()
        page.goto(APP + "/")
        page.get_by_test_id("dashboard-title").wait_for()
        assert page.url == f"{APP}/dashboards/{dash_id}", page.url

        page.goto(f"{APP}/dashboards")
        row = page.get_by_test_id("dashboard-row").filter(has_text="My board")
        row.get_by_test_id("dashboard-position").fill("3")
        row.get_by_role("button", name="Move").click()
        page.get_by_test_id("dashboard-row").filter(has_text="My board").wait_for()

        page.get_by_test_id("dashboard-row").filter(has_text="My board").get_by_test_id("delete-dashboard").click()
        page.wait_for_function(
            "() => ![...document.querySelectorAll('[data-testid=dashboard-row]')].some(r => r.innerText.includes('My board'))")
        page.goto(APP + "/")
        page.get_by_test_id("env-cards").wait_for()

    @check("team default fallback: fresh user sees banner and saves team dashboard")
    def _():
        # pre-clean in case a previous run died mid-check
        stale = sql("SELECT id FROM users WHERE email = 'pw-teamdefault@dev'")
        if stale:
            sql(f"DELETE FROM dashboards WHERE user_id = '{stale}'")
            sql(f"DELETE FROM team_members WHERE user_id = '{stale}'")
            sql(f"DELETE FROM user_roles WHERE user_id = '{stale}'")
            sql(f"DELETE FROM users WHERE id = '{stale}'")
        sql("""INSERT INTO users (email, password_hash, display_name)
               SELECT 'pw-teamdefault@dev', password_hash, 'pw-teamdefault@dev'
               FROM users WHERE email = 'sre@dev'""")
        uid = sql("SELECT id FROM users WHERE email = 'pw-teamdefault@dev'")
        sql(f"""INSERT INTO user_roles (user_id, role_id)
                SELECT '{uid}', r.id FROM roles r WHERE r.name = 'viewer'""")
        sql(f"""INSERT INTO team_members (team_id, user_id, team_role)
                SELECT t.id, '{uid}', 'member' FROM teams t WHERE t.name = 'sre'""")
        try:
            fresh = context.new_page()
            fresh.goto(f"{APP}/NewSession")
            fresh.get_by_test_id("login-email").fill("pw-teamdefault@dev")
            fresh.get_by_test_id("login-password").fill(password("sre"))
            fresh.get_by_test_id("login-submit").click()
            fresh.wait_for_url(APP + "/")
            fresh.get_by_test_id("team-default-banner").wait_for()
            fresh.get_by_test_id("save-team-default").click()
            fresh.get_by_test_id("dashboard-row").filter(has_text="Team default").wait_for()
            fresh.close()
        finally:
            sql(f"DELETE FROM dashboards WHERE user_id = '{uid}'")
            sql(f"DELETE FROM team_members WHERE user_id = '{uid}'")
            sql(f"DELETE FROM user_roles WHERE user_id = '{uid}'")
            sql(f"DELETE FROM users WHERE id = '{uid}'")
        # the fresh page's login overwrote the shared context cookie with a
        # now-deleted user; restore the sre session for later checks
        login(page, "sre")

    @check("theme switch swaps data-theme without reload and persists")
    def _():
        page.goto(f"{APP}/profile")
        page.get_by_test_id("theme-picker").wait_for()
        url_before = page.url
        with page.expect_response(lambda r: r.url.endswith("/profile/theme") and r.request.method == "POST"):
            page.get_by_test_id("theme-choice-dracula").click()
        page.wait_for_function("document.documentElement.dataset.theme === 'dracula'")
        assert page.url == url_before, f"unexpected navigation: {page.url}"
        page.reload()
        page.wait_for_function("document.documentElement.dataset.theme === 'dracula'")
        with page.expect_response(lambda r: r.url.endswith("/profile/theme") and r.request.method == "POST"):
            page.get_by_test_id("theme-choice-dark").click()
        page.wait_for_function("document.documentElement.dataset.theme === 'dark'")

    @check("theme snapshots: screenshot per theme pack")
    def _():
        tmpdir = os.environ.get("TMPDIR", "/tmp")
        snap_alert = sql("SELECT id FROM alerts ORDER BY created_at DESC LIMIT 1")
        assert snap_alert, "no alert to screenshot"
        for theme in ["latte", "frappe", "macchiato", "dracula", "light", "dark"]:
            resp = page.request.post(f"{APP}/profile/theme",
                                     data=json.dumps({"theme": theme}),
                                     headers={"Content-Type": "application/json"})
            assert resp.ok, f"theme {theme}: {resp.status}"
            page.goto(f"{APP}/alerts/{snap_alert}")
            page.get_by_test_id("alert-card").wait_for()
            path = os.path.join(tmpdir, f"theme-{theme}.png")
            page.screenshot(path=path, full_page=True)
            assert os.path.getsize(path) > 0, f"empty screenshot: {path}"

    # ---------------------------------------------------------- milestone 4

    @check("llm: analysis panel fills via websocket without reload")
    def _():
        fp = f"pw-llm-{int(time.time())}"
        fire_generic_alert(fp, title="pw disk pressure alert")
        alert_id = wait_sql_value(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'", 30)
        assert alert_id, "alert never arrived"
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("alert-card").wait_for()
        # the worker completes the analysis and pushes an "enriched" fragment
        page.get_by_test_id("llm-markdown").wait_for(timeout=90000)
        page.get_by_test_id("llm-probable-cause").wait_for()
        page.get_by_test_id("llm-actions").wait_for()
        page.get_by_test_id("llm-references").wait_for()
        footer = page.get_by_test_id("llm-footer").inner_text()
        assert "mock-llm-1" in footer and "v1" in footer, footer

    @check("llm: re-analyze appends, latest wins, older in history")
    def _():
        alert_id = sql("SELECT alert_id FROM llm_analyses WHERE status = 'done' ORDER BY created_at DESC LIMIT 1")
        assert alert_id, "no completed analysis"
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("llm-reanalyze").click()
        page.get_by_test_id("llm-markdown").wait_for()
        # NB: no dedupe assertion here — the grafana-absence reconcile can add
        # an event between the two runs, changing the prompt hash; identical-
        # context dedupe is covered deterministically in the integration suite
        deadline = time.time() + 90
        while time.time() < deadline:
            count = sql(f"SELECT COUNT(*) FROM llm_analyses WHERE alert_id = '{alert_id}' AND status = 'done'")
            if count and int(count) >= 2:
                break
            time.sleep(1)
        else:
            raise AssertionError("re-analyze never landed")
        page.reload()
        page.get_by_test_id("llm-history").wait_for()

    @check("llm: feedback up/down round-trip persists")
    def _():
        alert_id = sql("SELECT alert_id FROM llm_analyses WHERE status = 'done' ORDER BY created_at DESC LIMIT 1")
        analysis_id = sql(f"SELECT id FROM llm_analyses WHERE alert_id = '{alert_id}' AND status = 'done' ORDER BY created_at DESC LIMIT 1")
        page.goto(f"{APP}/alerts/{alert_id}")
        page.get_by_test_id("llm-feedback-up").first.click()
        page.get_by_test_id("llm-feedback-up").first.wait_for()
        score = wait_sql_value(f"SELECT score FROM llm_feedback WHERE analysis_id = '{analysis_id}'", 15)
        assert score == "1", score
        page.get_by_test_id("llm-feedback-down").first.click()
        deadline = time.time() + 15
        while time.time() < deadline:
            score = sql(f"SELECT score FROM llm_feedback WHERE analysis_id = '{analysis_id}'")
            if score == "-1":
                break
            time.sleep(1)
        assert score == "-1", score

    @check("llm: admin template edit bumps version; next analysis records v2")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin/llm")
        admin.get_by_test_id("llm-templates").wait_for()
        admin.get_by_test_id("llm-template-edit").first.click()
        admin.get_by_test_id("llm-template-form").wait_for()
        body = admin.get_by_test_id("llm-template-body").input_value()
        admin.get_by_test_id("llm-template-body").fill(body + "\nKeep answers short.")
        admin.get_by_test_id("llm-template-save").click()
        admin.get_by_test_id("llm-templates").wait_for()
        v2 = wait_sql_value("SELECT id FROM llm_prompt_templates WHERE name = 'alert_enrichment' AND version = 2", 15)
        assert v2, "v2 template row missing"
        # activate v2 (flip), fire, assert the analysis records version 2
        admin.get_by_test_id("llm-template-activate").first.click()
        admin.get_by_test_id("llm-templates").wait_for()
        assert sql("SELECT 1 FROM llm_prompt_templates WHERE name = 'alert_enrichment' AND version = 2 AND active"), "v2 not active"
        fp = f"pw-llmv2-{int(time.time())}"
        fire_generic_alert(fp, title="pw v2 template alert")
        deadline = time.time() + 90
        version = None
        while time.time() < deadline:
            version = sql(f"""SELECT a.prompt_version::text FROM llm_analyses a
                              JOIN alerts al ON al.id = a.alert_id
                              WHERE al.fingerprint = 'grafana:{fp}' AND a.status = 'done'""")
            if version:
                break
            time.sleep(1)
        assert version == "2", f"expected prompt_version 2, got {version!r}"
        # restore v1 active for the rest of the suite
        v1 = sql("SELECT id FROM llm_prompt_templates WHERE name = 'alert_enrichment' AND version = 1")
        sql(f"UPDATE llm_prompt_templates SET active = false WHERE name = 'alert_enrichment'")
        sql(f"UPDATE llm_prompt_templates SET active = true WHERE id = '{v1}'")
        admin.close()
        login(page, "sre")

    @check("llm: forced provider failure shows analysis unavailable")
    def _():
        urllib.request.urlopen(urllib.request.Request(
            "http://127.0.0.1:18084/debug/fail/500",
            data=json.dumps({"times": 10}).encode(),
            headers={"Content-Type": "application/json"}))
        try:
            fp = f"pw-llmfail-{int(time.time())}"
            fire_generic_alert(fp, title="pw failing analysis alert")
            alert_id = wait_sql_value(f"SELECT id FROM alerts WHERE fingerprint = 'grafana:{fp}'", 30)
            assert alert_id, "alert never arrived"
            # job path first: with 0s backoff all retries can finish before
            # the browser's websocket subscription is up, so don't rely on
            # the live push for this check
            deadline = time.time() + 120
            status = None
            while time.time() < deadline:
                status = sql(f"SELECT status FROM llm_analyses WHERE alert_id = '{alert_id}' ORDER BY created_at DESC LIMIT 1")
                if status in ("done", "failed"):
                    break
                time.sleep(1)
            assert status == "failed", f"analysis status: {status!r}"
            page.goto(f"{APP}/alerts/{alert_id}")
            page.get_by_test_id("llm-unavailable").wait_for(timeout=30000)
            kinds = page.get_by_test_id("alert-timeline").inner_text()
            assert "llm_failed" in kinds, kinds
        finally:
            urllib.request.urlopen(urllib.request.Request(
                "http://127.0.0.1:18084/debug/reset", data=b"{}",
                headers={"Content-Type": "application/json"}))

    # ---------------------------------------------------------- milestone 5

    @check("source health: dashboard shows the internal alert without reload")
    def _():
        page.goto(APP + "/")
        page.get_by_test_id("env-cards").wait_for()
        card = page.get_by_test_id("env-card").filter(has_text="dev").first
        before = card.get_by_test_id("count-firing").inner_text()
        src = sql("""INSERT INTO sources (type, name, base_url, poll_interval_seconds, config)
                     VALUES ('zabbix', 'pw-health-source', 'http://127.0.0.1:9', 5,
                             '{"tokenEnv":"ZABBIX_TOKEN"}'::jsonb) RETURNING id""")
        src_id = inserted_id(src)
        try:
            deadline = time.time() + 180
            while time.time() < deadline:
                if sql(f"SELECT 1 FROM alerts WHERE fingerprint = 'halemans:source-health:{src_id}' AND status = 'firing' LIMIT 1"):
                    break
                time.sleep(1)
            else:
                raise AssertionError("source-health alert never appeared")
            deadline = time.time() + 30
            while time.time() < deadline:
                now_text = card.get_by_test_id("count-firing").inner_text()
                if now_text != before:
                    break
                time.sleep(1)
            else:
                raise AssertionError(f"dashboard count never moved (was {before!r})")
        finally:
            # the alert row references the source; disabling stops the flapping
            sql(f"UPDATE sources SET enabled = false WHERE id = '{src_id}'")
    def _():
        src = sql("""INSERT INTO sources (type, name, base_url, consecutive_failures, last_error, next_poll_at)
                     VALUES ('webhook', 'pw-flaky-source', 'http://127.0.0.1:9', 3, 'boom', NOW() + interval '10 min')
                     RETURNING id""")
        src_id = inserted_id(src)
        try:
            admin = context.new_page()
            login(admin, "admin")
            admin.goto(f"{APP}/sources")
            row = admin.get_by_test_id("source-row").filter(has_text="pw-flaky-source")
            row.wait_for()
            assert "failing" in row.get_by_test_id("source-health").inner_text()
            assert row.get_by_test_id("source-failures").inner_text() == "3"
            assert "boom" in row.get_by_test_id("source-last-error").inner_text()
            sql(f"UPDATE sources SET consecutive_failures = 0, last_error = NULL, next_poll_at = NULL WHERE id = '{src_id}'")
            admin.goto(f"{APP}/sources")
            row = admin.get_by_test_id("source-row").filter(has_text="pw-flaky-source")
            row.wait_for()
            assert "healthy" in row.get_by_test_id("source-health").inner_text()
            admin.close()
        finally:
            sql(f"DELETE FROM sources WHERE id = '{src_id}'")

    @check("audit export: download + export log row, viewer denied")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        resp = admin.request.get(f"{APP}/admin/audit/export?format=csv")
        assert resp.status == 200, resp.status
        assert "attachment" in (resp.headers.get("content-disposition") or "")
        body = resp.text()
        assert body.startswith("event_id,created_at,alert_id"), body[:120]
        row_count = wait_sql_value("SELECT row_count::text FROM audit_exports WHERE format = 'csv' ORDER BY created_at DESC LIMIT 1", 15)
        assert row_count and int(row_count) >= 0, row_count
        admin.goto(f"{APP}/admin/audit")
        admin.get_by_test_id("audit-exports-table").wait_for()
        admin.get_by_test_id("audit-export-row").first.wait_for()
        admin.close()
        viewer = context.new_page()
        login(viewer, "viewer")
        denied = viewer.evaluate("""async () => {
            const res = await fetch('/admin/audit/export?format=csv');
            return res.status;
        }""")
        assert denied == 403, denied
        viewer.close()
        login(page, "sre")

    @check("admin job metrics page renders counters and failures table")
    def _():
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin")
        admin.get_by_test_id("job-metrics-table").wait_for()
        rows = admin.get_by_test_id("job-metrics-row").all()
        assert len(rows) >= 10, f"metrics rows: {len(rows)}"
        admin.get_by_test_id("job-failures-table").wait_for()
        admin.close()
        login(page, "sre")

    # milestone 6: token management UI (design_docs/milestone_6.md §4/§9)
    @check("profile API tokens: create (shown once), use, revoke")
    def _():
        page.goto(f"{APP}/profile")
        page.get_by_test_id("api-token-create-form").wait_for()
        page.get_by_test_id("api-token-name").fill("pw-token")
        page.get_by_test_id("scope_metrics").check()
        page.get_by_test_id("api-token-create").click()
        # plaintext shown exactly once post-creation (rendered, not redirected)
        page.get_by_test_id("api-token-plaintext").wait_for()
        token = page.get_by_test_id("api-token-plaintext").inner_text().strip()
        assert len(token) > 20, token
        row = page.get_by_test_id("api-token-row").filter(has_text="pw-token")
        row.wait_for()
        # reload: the plaintext banner is gone
        page.goto(f"{APP}/profile")
        page.get_by_test_id("api-tokens-table").wait_for()
        assert page.get_by_test_id("api-token-plaintext").count() == 0
        # bearer call works; last_used_at fills
        req = urllib.request.Request(f"{APP}/api/v1/alerts?limit=1",
                                     headers={"Authorization": f"Bearer {token}"})
        with urllib.request.urlopen(req) as resp:
            assert resp.status == 200, resp.status
            assert '"alerts"' in resp.read().decode()
        used = wait_sql_value(f"SELECT last_used_at IS NOT NULL FROM api_tokens WHERE prefix = '{token[:8]}'", 15)
        assert used == "t", used
        # metrics scope works too
        req = urllib.request.Request(f"{APP}/metrics",
                                     headers={"Authorization": f"Bearer {token}"})
        with urllib.request.urlopen(req) as resp:
            assert resp.status == 200, resp.status
        # revoke via the UI; the token dies immediately
        page.get_by_test_id("api-token-row").filter(has_text="pw-token") \
            .get_by_test_id("api-token-revoke").click()
        page.get_by_test_id("api-token-row").filter(has_text="pw-token") \
            .get_by_test_id("api-token-revoked").wait_for()
        try:
            urllib.request.urlopen(urllib.request.Request(
                f"{APP}/api/v1/alerts", headers={"Authorization": f"Bearer {token}"}))
            raise AssertionError("expected 401 after revoke")
        except urllib.error.HTTPError as e:
            assert e.code == 401, e.code

    @check("admin revokes any user's API token")
    def _():
        token = os.environ.get("HALEMANS_API_TOKEN")
        if not token:
            token = open(os.path.join(STATE, "halemans", "api-token")).read().strip()
        admin = context.new_page()
        login(admin, "admin")
        admin.goto(f"{APP}/admin")
        admin.get_by_test_id("admin-api-tokens-table").wait_for()
        row = admin.get_by_test_id("admin-api-token-row").filter(has_text=token[:8])
        row.get_by_test_id("admin-api-token-revoke").click()
        row.get_by_text("revoked").wait_for()
        admin.close()
        try:
            urllib.request.urlopen(urllib.request.Request(
                f"{APP}/api/v1/alerts", headers={"Authorization": f"Bearer {token}"}))
            raise AssertionError("expected 401 after admin revoke")
        except urllib.error.HTTPError as e:
            assert e.code == 401, e.code
        login(page, "sre")

    browser.close()

print()
if failures:
    print(f"playwright: {len(failures)} failure(s)")
    sys.exit(1)
print("playwright: all scenarios passed")
