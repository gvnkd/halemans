module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import Generated.Types
import Web.Types
import Web.Routes
import Application.Helper.View
import Application.Helper.Controller () -- CurrentUserRecord instance for currentUserOrNothing
import Application.Helper.Theme (themeFromSettings, bsTheme)
import Application.Version (appVersion)

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en" data-theme={activeTheme} data-bs-theme={activeBsTheme}>
    <head>
        {metaTags}

        {stylesheets}
        {scripts}

        <title>{pageTitleOrDefault "Halemans"}</title>
    </head>
    <body>
        {navigation}
        <div class="container mt-4">
            {renderFlashMessages}
            <div id="push-banner" class="alert alert-warning d-none" data-testid="push-banner"></div>
            {inner}
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

navigation :: Html
navigation = [hsx|
<nav class="navbar navbar-expand-lg" data-testid="nav">
    <div class="container-fluid">
        <div class="d-flex flex-column">
            <a class="navbar-brand" href={DashboardAction}>Halemans</a>
            <span class="badge app-version-badge" data-testid="app-version">v{appVersion}</span>
        </div>
        <ul class="navbar-nav me-auto">
            <li class="nav-item"><a class="nav-link" href={DashboardAction}>Dashboard</a></li>
            <li class="nav-item"><a class="nav-link" href={DashboardsAction}>Dashboards</a></li>
            <li class="nav-item"><a class="nav-link" href={AlertsAction}>Alerts</a></li>
            <li class="nav-item"><a class="nav-link" href={BlackoutsAction}>Blackouts</a></li>
            <li class="nav-item"><a class="nav-link" href={SourcesAction}>Sources</a></li>
            <li class="nav-item dropdown">
                <a class="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown">Admin</a>
                <ul class="dropdown-menu">
                    <li><a class="dropdown-item" href={TeamsAction}>Teams</a></li>
                    <li><a class="dropdown-item" href={GroupingRulesAction}>Grouping rules</a></li>
                    <li><a class="dropdown-item" href={NotificationRulesAction}>Notification rules</a></li>
                    <li><a class="dropdown-item" href={EscalationPoliciesAction}>Escalation policies</a></li>
                    <li><a class="dropdown-item" href={IntegrationsAction}>Integrations</a></li>
                    <li><a class="dropdown-item" href={LlmAdminAction}>LLM</a></li>
                    <li><a class="dropdown-item" href={AssetsAdminAction}>Assets</a></li>
                    <li><a class="dropdown-item" href={LlmQueueAction}>LLM queue</a></li>
                    <li><a class="dropdown-item" href={AdminAction}>Jobs</a></li>
                    <li><a class="dropdown-item" href={AuditExportsAction}>Audit exports</a></li>
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
    Just user -> [hsx|
        <li class="nav-item"><a class="nav-link" href={ProfileAction}>{user.email}</a></li>
        <li class="nav-item"><a class="nav-link js-delete js-delete-no-confirm" href={DeleteSessionAction} data-testid="logout">Logout</a></li>
    |]
    Nothing -> [hsx|
        <li class="nav-item"><a class="nav-link" href={NewSessionAction}>Login</a></li>
    |]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
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
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Halemans"/>
    {autoRefreshMeta}
|]
