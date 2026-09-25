module Web.View.Layout (defaultLayout, Html) where

import Application.Helper.Controller ()

-- CurrentUserRecord instance for currentUserOrNothing
import Application.Helper.Theme (bsTheme, themeFromSettings)
import Application.Helper.Timezone (timezoneFromSettings)
import Application.Helper.View
import Application.Service.I18n (languageCode)
import Application.Version (appVersion)
import qualified Data.Text as Text
import Generated.Types
import IHP.Environment
import IHP.RouterSupport (HasPath)
import IHP.ViewPrelude
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner =
    [hsx|
<!DOCTYPE html>
<html lang={activeLanguage} data-theme={activeTheme} data-bs-theme={activeBsTheme} data-tz={activeTimezone}>
    <head>
        {metaTags activeTheme}

        {stylesheets}
        {scripts}

        <title>{pageTitleOrDefault "Halemans"}</title>
    </head>
    <body>
        <a href="#content" class="skip-to-content">{tr "Skip to content"}</a>
        {navigation}
        <div class="page-messages">
            {renderFlashMessages}
            <div id="push-banner" class="alert alert-warning d-none" data-testid="push-banner"></div>
        </div>
        <main id="content">
            {inner}
        </main>
        {agentWidget}
    </body>
</html>
|]
  where
    activeTheme :: Text
    activeTheme = case currentUserOrNothing of
        Just user -> themeFromSettings user.settings
        Nothing -> "dark"
    activeBsTheme :: Text
    activeBsTheme = bsTheme activeTheme
    activeLanguage :: Text
    activeLanguage = languageCode currentLanguage
    -- Fixed offset from users.settings.timezone ("" = browser default);
    -- app.js reads data-tz when localizing <time class="utc-time"> elements.
    activeTimezone :: Text
    activeTimezone = case currentUserOrNothing of
        Just user -> fromMaybe "" (timezoneFromSettings user.settings)
        Nothing -> ""

navigation :: Html
navigation =
    [hsx|
    <nav class="navbar navbar-expand-lg" data-testid="nav">
    <div class="container-fluid px-4">
        <div class="d-flex align-items-center">
            <a class="navbar-brand" href={DashboardAction}><span class="brand-word">Hale</span><span class="brand-word-accent">mans</span></a>
            <span class="app-version-badge" data-testid="app-version">v{appVersion}</span>
        </div>
        <ul class="navbar-nav app-nav flex-wrap">
            {navLink DashboardAction (tr "Overview") "nav-overview"}
            {navLink DashboardsAction (tr "Dashboards") "nav-dashboards"}
            {navLink AlertsAction (tr "Alerts") "nav-alerts"}
            {navLink ReportsAction (tr "Reports") "nav-reports"}
            {navLink BlackoutsAction (tr "Blackouts") "nav-blackouts"}
            {navLink SourcesAction (tr "Sources") "nav-sources"}
            <li class="nav-item dropdown">
                <a class="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown">{tr "Admin"}</a>
                <ul class="dropdown-menu">
                    <li><a class="dropdown-item" href={TeamsAction}>{tr "Teams"}</a></li>
                    <li><a class="dropdown-item" href={GroupingRulesAction}>{tr "Grouping rules"}</a></li>
                    <li><a class="dropdown-item" href={FieldMappingsAction}>{tr "Field mappings"}</a></li>
                    <li><a class="dropdown-item" href={NotificationRulesAction}>{tr "Notification rules"}</a></li>
                    <li><a class="dropdown-item" href={EscalationPoliciesAction}>{tr "Escalation policies"}</a></li>
                    <li><a class="dropdown-item" href={IntegrationsAction}>{tr "Integrations"}</a></li>
                    <li><a class="dropdown-item" href={LlmAdminAction}>LLM</a></li>
                    <li><a class="dropdown-item" href={LlmAgentConfigAction}>{tr "Agent"}</a></li>
                    <li><a class="dropdown-item" href={AssetsAdminAction}>{tr "Assets"}</a></li>
                    <li><a class="dropdown-item" href={LlmQueueAction}>{tr "LLM queue"}</a></li>
                    <li><a class="dropdown-item" href={AdminAction}>{tr "Jobs"}</a></li>
                    <li><a class="dropdown-item" href={AdminDatabaseAction}>{tr "Database"}</a></li>
                    <li><a class="dropdown-item" href={AuditExportsAction}>{tr "Audit exports"}</a></li>
                    <li><a class="dropdown-item" href={FlappingAction}>{tr "Flapping"}</a></li>
                </ul>
            </li>
            {userMenu}
        </ul>
    </div>
</nav>
|]

-- Floating agent chat widget on every page (internal API milestone). Logged-in
-- only: the chat endpoints are session-authed and act as the current user.
-- All behavior lives in app.js (CSP: no inline handlers); the data attributes
-- carry the endpoint URLs and page context is gathered client-side on send.
agentWidget :: Html
agentWidget = case currentUserOrNothing of
    Nothing -> mempty
    Just _ ->
        [hsx|
        <div id="agent-widget" class="agent-widget" data-chat-url="/agent/chat" data-history-url="/agent/chat" data-sessions-url="/agent/sessions" data-new-chat-label={tr "New chat"} data-testid="agent-widget">
            <button type="button" id="agent-toggle" class="agent-fab" data-testid="agent-toggle" aria-expanded="false">{tr "Ask agent"}</button>
            <section id="agent-panel" class="agent-panel d-none" data-testid="agent-panel" role="dialog" aria-label={tr "Halemans agent"}>
                <header class="agent-panel-header">
                    <span>{tr "Halemans agent"}</span>
                    <div class="agent-panel-controls">
                        <select id="agent-sessions" class="agent-sessions" data-testid="agent-sessions" aria-label={tr "Chat history"}></select>
                        <button type="button" id="agent-new-chat" class="agent-new-chat" data-testid="agent-new-chat" title={tr "New chat"} aria-label={tr "New chat"}>+</button>
                    </div>
                    <button type="button" id="agent-close" class="agent-close" data-testid="agent-close" aria-label={tr "Close"}>×</button>
                </header>
                <div id="agent-messages" class="agent-messages" data-testid="agent-messages"></div>
                <div class="agent-form">
                    <textarea id="agent-input" class="agent-input" data-testid="agent-input" placeholder={tr "Ask about this page…"} rows="2" autocomplete="off"></textarea>
                    <button type="button" id="agent-send" class="btn-brand agent-send" data-testid="agent-send">{tr "Send"}</button>
                </div>
            </section>
        </div>
    |]

userMenu :: Html
userMenu = case currentUserOrNothing of
    Just user ->
        [hsx|
        <li class="nav-item dropdown nav-user">
            <a class="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" data-testid="user-menu">{user.email}</a>
            <ul class="dropdown-menu dropdown-menu-end">
                <li><a class="dropdown-item" href={ProfileAction}>{tr "Profile"}</a></li>
                <li><hr class="dropdown-divider"/></li>
                <li><a class="dropdown-item js-delete js-delete-no-confirm" href={DeleteSessionAction} data-testid="logout">{tr "Logout"}</a></li>
            </ul>
        </li>
    |]
    Nothing ->
        [hsx|
        <li class="nav-item"><a class="nav-link" href={NewSessionAction}>{tr "Login"}</a></li>
    |]

-- Top-level nav link with active-page marker: the link gets .active when the
-- current path is the link's own path (exact) or sits under it (e.g.
-- /dashboards/<id>/edit marks Dashboards). Root only ever matches exactly.
navLink :: (?context :: Request, ?request :: Request, HasPath action) => action -> Text -> Text -> Html
navLink action label testId =
    [hsx|
        <li class="nav-item">
            <a class={linkClass} href={pathTo action} data-testid={testId}>{label}</a>
        </li>
    |]
  where
    target :: Text
    target = pathTo action

    linkClass :: Text
    linkClass =
        if isActivePath target || (target /= "/" && isActivePathOrSub target)
            then "nav-link active"
            else "nav-link"

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets =
    [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts =
    [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-4.0.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/halemans-live.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts =
    [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Text -> Html
metaTags activeTheme =
    [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta name="description" content="Alert aggregation and enrichment dashboard for Zabbix, Grafana, Alertmanager and generic webhooks"/>
    <meta name="theme-color" content={themeColor activeTheme}/>
    <meta property="og:title" content="Halemans"/>
    <meta property="og:type" content="website"/>
    <meta property="og:description" content="Alert aggregation and enrichment dashboard for Zabbix, Grafana, Alertmanager and generic webhooks"/>
    <meta property="og:image" content={ogImageUrl}/>
    <meta property="og:image:width" content="1200"/>
    <meta property="og:image:height" content="630"/>
    <meta name="twitter:card" content="summary_large_image"/>
    <link rel="manifest" href={assetPath "/manifest.webmanifest"}/>
    <link rel="icon" type="image/svg+xml" href={assetPath "/halemans-micromark-ondark.svg"}/>
    <link rel="icon" href={assetPath "/favicon.ico"} sizes="any"/>
    <link rel="icon" type="image/png" sizes="32x32" href={assetPath "/halemans-favicon-32.png"}/>
    <link rel="icon" type="image/png" sizes="16x16" href={assetPath "/halemans-favicon-16.png"}/>
    <link rel="apple-touch-icon" href={assetPath "/halemans-app-icon-180.png"}/>
    {autoRefreshMeta}
|]

-- og:image must be an absolute URL (scrapers ignore relative ones);
-- frameworkConfig.baseUrl is IHP_BASEURL or http://hostname:port.
ogImageUrl :: (?context :: Request, ?request :: Request) => Text
ogImageUrl = ?context.frameworkConfig.baseUrl <> assetPath "/halemans-og-card-1200x630.png"

-- PWA/status-bar color follows the active pack's --bg (static/app.css);
-- mirrors themeFromSettings' default of "dark" for anonymous users.
themeColor :: Text -> Text
themeColor theme = case theme of
    "latte" -> "#eff1f5"
    "light" -> "#eff1f5"
    "frappe" -> "#303446"
    "macchiato" -> "#24273a"
    "dracula" -> "#282a36"
    "halemans-dark" -> "#0C121D"
    "halemans-light" -> "#F5F3EE"
    _ -> "#1e1e2e"
