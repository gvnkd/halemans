module Web.View.Reports.Index where

import qualified Application.Service.Reports as Reports
import Web.View.Fragments (emptyStateHtml, filterMultiSelect, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView
    { rangeFrom :: Text
    , rangeTo :: Text
    , envs :: [Text]
    , selectedEnv :: Maybe Text
    , severities :: [Text]
    , severityOptions :: [Text]
    , volumeSvg :: Text
    , volumeTitle :: Text
    , volumeBucket :: Text
    , volumeSeverities :: [Text]
    , hasEmptyBuckets :: Bool
    , totalAlerts :: Int64
    , criticalCount :: Int64
    , highCount :: Int64
    , topBreakdownLabel :: Text
    , topBreakdownCount :: Int64
    , topBreakdownPct :: Integer
    , avgResolution :: Maybe Double
    , severityBars :: [(Text, Text, Double, Text)]
    , envBars :: [(Text, Text, Double, Text)]
    , envPanelTitle :: Text
    , mttrBars :: [(Text, Text, Double, Text)]
    , sourceBars :: [(Text, Text, Double, Text)]
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Reports") mempty}
        <p class="page-subtitle">{tr "Alert volume and response trends across all sources."}</p>
        <div class="report-kpis" data-testid="report-kpis">
            <div class="report-kpi">
                <div class="report-kpi-label">{tr "Total alerts"}</div>
                <div class="report-kpi-value"><span class="mono">{totalAlerts}</span></div>
            </div>
            <div class="report-kpi">
                <div class="report-kpi-label">{tr "Critical / high"}</div>
                <div class="report-kpi-value"><span class="mono">{criticalCount}</span> <small>/</small> <span class="mono">{highCount}</span></div>
            </div>
            <div class="report-kpi">
                <div class="report-kpi-label">{if isJust selectedEnv then tr "Top host" else tr "Top environment"}</div>
                <div class="report-kpi-value">{topBreakdownLabel} <small class="mono">{topBreakdownCount} · {topBreakdownPct}%</small></div>
            </div>
            <div class="report-kpi">
                <div class="report-kpi-label">{tr "Avg resolution"}</div>
                <div class="report-kpi-value">{avgResolutionValue}</div>
            </div>
        </div>
        <form method="GET" action={ReportsAction} class="report-toolbar" data-testid="reports-form">
            <div class="fld">
                <label>{tr "Window"}</label>
                <!-- preset only: unnamed (never submitted); app.js copies the
                     picked expression into from/to and resets this to Custom
                     on any manual from/to edit -->
                <select class="select" data-window-preset="" data-testid="reports-window">
                    <option value="" selected="selected">{tr "Custom"}</option>
                    <option value="now() - 24h">24h</option>
                    <option value="now() - 168h">7d</option>
                    <option value="now() - 720h">30d</option>
                </select>
            </div>
            <div class="fld fld-grow">
                <label>{tr "From"}</label>
                <input type="hidden" name="from" value={rangeFrom}/>
                <div class="input-group">
                    <input class="form-control" placeholder={tr "now() - 7d or pick a date"} value={rangeFrom} data-local-datetime="from" data-testid="reports-from"/>
                    <button type="button" class="btn btn-ghost" data-calendar-toggle="from" data-testid="reports-from-calendar" aria-label={tr "Pick from date"}>{calendarIcon}</button>
                </div>
            </div>
            <div class="fld fld-grow">
                <label>{tr "To"}</label>
                <input type="hidden" name="to" value={rangeTo}/>
                <div class="input-group">
                    <input class="form-control" placeholder={tr "now() or pick a date"} value={rangeTo} data-local-datetime="to" data-testid="reports-to"/>
                    <button type="button" class="btn btn-ghost" data-calendar-toggle="to" data-testid="reports-to-calendar" aria-label={tr "Pick to date"}>{calendarIcon}</button>
                </div>
            </div>
            <div class="fld">
                <label>{tr "Environment"}</label>
                <select name="env" class="select" data-testid="reports-env">
                    {envOption selectedEnv ""}
                    {forEach envs (envOption selectedEnv)}
                </select>
            </div>
            <div class="fld">
                <label>{tr "Volume bucket"}</label>
                <select name="bucket" class="select" data-testid="reports-bucket">
                    {forEach ["", "hour", "day"] (bucketOption volumeBucket)}
                </select>
            </div>
            <div class="fld">
                <label>{tr "severity"}</label>
                {filterMultiSelect "severity" (tr "severity") severityOptions severities}
            </div>
            <div class="fld">
                <button type="submit" class="btn btn-brand" data-testid="reports-submit">{tr "Render"}</button>
            </div>
        </form>
        <div class="card report-panel" data-testid="report-volume">
            <div class="card-body report-chart">
                <div class="report-panel-head">
                    <h2>{volumeTitle}</h2>
                    {volumeLegend}
                </div>
                {preEscapedToHtml volumeSvg}
                {axisNote}
            </div>
        </div>
        <div class="report-grid-2">
            {barsPanel "report-severity" (tr "Alerts by severity") severityNote severityBars}
            {barsPanel "report-env" envPanelTitle mempty envBars}
        </div>
        <div class="report-grid-2">
            {barsPanel "report-mttr" (tr "Mean time to resolve") mttrNote mttrBars}
            {barsPanel "report-sources" (tr "Top sources") sourcesNote sourceBars}
        </div>
    |]
      where
        severityNote = [hsx|<span>{trp "share of {n}" [("n", tshow totalAlerts)]}</span>|]
        mttrNote = [hsx|<span>{tr "by severity"}</span>|]
        sourcesNote = [hsx|<span>{tr "alerts ingested"}</span>|]
        avgResolutionValue = case avgResolution of
            Just secs -> [hsx|<span class="mono">{Reports.formatHours secs}</span> <small>h</small>|]
            Nothing -> [hsx|—|]
        axisNote =
            if hasEmptyBuckets
                then [hsx|<div class="report-axis-note" data-testid="report-volume-axis">{tr "Empty buckets are shown as baseline ticks, not zero-height bars."}</div>|]
                else mempty
        volumeLegend =
            if null volumeSeverities
                then mempty
                else [hsx|<div class="report-legend" data-testid="report-volume-legend">{forEach volumeSeverities legendItem}</div>|]
        legendItem severity =
            [hsx|<span><i class={"swatch " <> Reports.severityFillClass severity}></i>{severity}</span>|]

barsPanel :: Text -> Text -> Html -> [(Text, Text, Double, Text)] -> Html
barsPanel testId title note bars =
    [hsx|
    <div class="card report-panel" data-testid={testId}>
        <div class="card-body report-bars">
            <div class="report-panel-head">
                <h2>{title}</h2>
                <span class="report-legend">{note}</span>
            </div>
            {barsBody}
        </div>
    </div>
|]
  where
    barsBody =
        if null bars
            then emptyStateHtml (testId <> "-empty") (tr "No alerts in the selected window.")
            else forEach bars barRowHtml

barRowHtml :: (Text, Text, Double, Text) -> Html
barRowHtml (name, valueText, pct, fillClass) =
    [hsx|
    <div class="bar-row">
        <span class="name" title={name}>{Reports.truncateLabel 12 name}</span>
        <div class="track"><div class={"fill " <> fillClass} style={"width: " <> Reports.formatPct pct}></div></div>
        <span class="val">{valueText}</span>
    </div>
|]

bucketOption :: Text -> Text -> Html
bucketOption selected value =
    let label = case value of
            "" -> tr "Auto" :: Text
            "hour" -> tr "Per hour"
            "day" -> tr "Per day"
            other -> other
     in if value == selected
            then [hsx|<option value={value} selected="selected">{label}</option>|]
            else [hsx|<option value={value}>{label}</option>|]

calendarIcon :: Html
calendarIcon =
    [hsx|
    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">
        <path d="M3.5 0a.5.5 0 0 1 .5.5V1h8V.5a.5.5 0 0 1 1 0V1h1a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V3a2 2 0 0 1 2-2h1V.5a.5.5 0 0 1 .5-.5zM1 4v10a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V4H1z"/>
    </svg>
|]

envOption :: Maybe Text -> Text -> Html
envOption selected value =
    let label = if value == "" then tr "All environments" else value
     in if Just value == selected || (value == "" && isNothing selected)
            then [hsx|<option value={value} selected="selected">{label}</option>|]
            else [hsx|<option value={value}>{label}</option>|]
