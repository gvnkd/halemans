module Web.View.Layout (defaultLayout, Html) where

import Application.Helper.Controller ()

-- CurrentUserRecord instance for currentUserOrNothing
import Application.Helper.Theme (bsTheme, themeFromSettings)
import Application.Helper.Timezone (timezoneFromSettings)
import Application.Helper.View
import Application.Service.I18n (languageCode)
import Application.Version (appVersion)
import Generated.Types
import IHP.Environment
import IHP.ViewPrelude
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner =
    [hsx|
<!DOCTYPE html>
<html lang={activeLanguage} data-theme={activeTheme} data-bs-theme={activeBsTheme} data-tz={activeTimezone}>
    <head>
        {metaTags}

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
        <div class="d-flex flex-column">
            <a class="navbar-brand" href={DashboardAction}><img src={assetPath "/halemans-app-icon-192.png"} alt="" class="navbar-glyph"/>Halemans</a>
            <span class="badge app-version-badge" data-testid="app-version">v{appVersion}</span>
        </div>
        <ul class="navbar-nav me-auto">
            <li class="nav-item"><a class="nav-link" href={DashboardAction}>{tr "Overview"}</a></li>
            <li class="nav-item"><a class="nav-link" href={DashboardsAction}>{tr "Dashboards"}</a></li>
            <li class="nav-item"><a class="nav-link" href={AlertsAction}>{tr "Alerts"}</a></li>
            <li class="nav-item"><a class="nav-link" href={ReportsAction} data-testid="nav-reports">{tr "Reports"}</a></li>
            <li class="nav-item"><a class="nav-link" href={BlackoutsAction}>{tr "Blackouts"}</a></li>
            <li class="nav-item"><a class="nav-link" href={SourcesAction}>{tr "Sources"}</a></li>
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
        <li class="nav-item"><a class="nav-link" href={ProfileAction}>{user.email}</a></li>
        <li class="nav-item"><a class="nav-link js-delete js-delete-no-confirm" href={DeleteSessionAction} data-testid="logout">{tr "Logout"}</a></li>
    |]
    Nothing ->
        [hsx|
        <li class="nav-item"><a class="nav-link" href={NewSessionAction}>{tr "Login"}</a></li>
    |]

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

metaTags :: Html
metaTags =
    [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta name="description" content="Alert aggregation and enrichment dashboard for Zabbix, Grafana, Alertmanager and generic webhooks"/>
    <meta name="theme-color" content="#2a2a3c"/>
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
