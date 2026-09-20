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
        <div class="container-fluid mt-4 px-4">
            {renderFlashMessages}
            <div id="push-banner" class="alert alert-warning d-none" data-testid="push-banner"></div>
            <main id="content">
                {inner}
            </main>
        </div>
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
    <div class="container-fluid">
        <div class="d-flex align-items-center">
            <a class="navbar-brand" href={DashboardAction}><img src={assetPath "/halemans-app-icon-192.png"} alt="" class="navbar-glyph"/>Halemans</a>
            <span class="app-version-badge" data-testid="app-version">v{appVersion}</span>
        </div>
        <ul class="navbar-nav me-auto">
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
                    <li><a class="dropdown-item" href={AssetsAdminAction}>{tr "Assets"}</a></li>
                    <li><a class="dropdown-item" href={LlmQueueAction}>{tr "LLM queue"}</a></li>
                    <li><a class="dropdown-item" href={AdminAction}>{tr "Jobs"}</a></li>
                    <li><a class="dropdown-item" href={AdminDatabaseAction}>{tr "Database"}</a></li>
                    <li><a class="dropdown-item" href={AuditExportsAction}>{tr "Audit exports"}</a></li>
                    <li><a class="dropdown-item" href={FlappingAction}>{tr "Flapping"}</a></li>
                </ul>
            </li>
        </ul>
        <ul class="navbar-nav">
            {userMenu}
        </ul>
    </div>
</nav>
|]

userMenu :: Html
userMenu = case currentUserOrNothing of
    Just user ->
        [hsx|
        <li class="nav-item dropdown">
            <a class="nav-link user-chip dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" data-testid="user-menu">
                <span class="user-chip-avatar" aria-hidden="true">{initialsOf user.email}</span>
                <span class="user-chip-name">{user.email}</span>
            </a>
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

-- Avatar initials from the local part of the email: "sara.reyes@x" -> "SR",
-- "sara@x" -> "S". Falls back to "?" for empty input.
initialsOf :: Text -> Text
initialsOf email =
    let local = fst (Text.breakOn "@" email)
        parts = filter (not . Text.null) (Text.split (== '.') local)
        initials = Text.concat (map (Text.take 1) (take 2 parts))
     in if Text.null initials then "?" else Text.toUpper initials

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
    <meta property="og:image" content={assetPath "/halemans-app-icon-512.png"}/>
    <meta name="twitter:card" content="summary"/>
    <link rel="manifest" href={assetPath "/manifest.webmanifest"}/>
    <link rel="icon" href={assetPath "/favicon.ico"} sizes="any"/>
    <link rel="icon" type="image/png" sizes="32x32" href={assetPath "/halemans-favicon-32.png"}/>
    <link rel="icon" type="image/png" sizes="16x16" href={assetPath "/halemans-favicon-16.png"}/>
    <link rel="apple-touch-icon" href={assetPath "/halemans-app-icon-180.png"}/>
    {autoRefreshMeta}
|]

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
