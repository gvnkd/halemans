module Web.View.Reports.Index where

import Web.View.Fragments (filterMultiSelect)
import Web.View.Prelude

data IndexView = IndexView
    { rangeFrom :: Text
    , rangeTo :: Text
    , envs :: [Text]
    , selectedEnv :: Maybe Text
    , severities :: [Text]
    , severityOptions :: [Text]
    , severitySvg :: Text
    , breakdownSvg :: Text
    , breakdownTitle :: Text
    , volumeSvg :: Text
    , volumeTitle :: Text
    , volumeBucket :: Text
    , volumeSeverities :: [Text]
    , mttrSvg :: Text
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        <h1>Reports</h1>
        <form method="GET" action={ReportsAction} class="row g-2 align-items-end mb-4" data-testid="reports-form">
            <div class="col-auto">
                <label class="form-label">Window</label>
                <!-- preset only: unnamed (never submitted); app.js copies the
                     picked expression into from/to and resets this to Custom
                     on any manual from/to edit -->
                <select class="form-select" data-window-preset="" data-testid="reports-window">
                    <option value="" selected="selected">Custom</option>
                    <option value="now() - 24h">24h</option>
                    <option value="now() - 168h">7d</option>
                    <option value="now() - 720h">30d</option>
                </select>
            </div>
            <div class="col-auto">
                <label class="form-label">From</label>
                <input type="hidden" name="from" value={rangeFrom}/>
                <div class="input-group">
                    <input class="form-control" placeholder="now() - 7d or pick a date" value={rangeFrom} data-local-datetime="from" data-testid="reports-from"/>
                    <button type="button" class="btn btn-outline-secondary" data-calendar-toggle="from" data-testid="reports-from-calendar" aria-label="Pick from date">{calendarIcon}</button>
                </div>
            </div>
            <div class="col-auto">
                <label class="form-label">To</label>
                <input type="hidden" name="to" value={rangeTo}/>
                <div class="input-group">
                    <input class="form-control" placeholder="now() or pick a date" value={rangeTo} data-local-datetime="to" data-testid="reports-to"/>
                    <button type="button" class="btn btn-outline-secondary" data-calendar-toggle="to" data-testid="reports-to-calendar" aria-label="Pick to date">{calendarIcon}</button>
                </div>
            </div>
            <div class="col-auto">
                <label class="form-label">Environment</label>
                <select name="env" class="form-select" data-testid="reports-env">
                    {envOption selectedEnv ""}
                    {forEach envs (envOption selectedEnv)}
                </select>
            </div>
            <div class="col-auto">
                <label class="form-label">Volume bucket</label>
                <select name="bucket" class="form-select" data-testid="reports-bucket">
                    {forEach ["", "hour", "day"] (bucketOption volumeBucket)}
                </select>
            </div>
            <div class="col-auto d-flex align-items-end">
                {filterMultiSelect "severity" "severity" severityOptions severities}
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-primary" data-testid="reports-submit">Render</button>
            </div>
        </form>
        <div class="card mb-4" data-testid="report-volume">
            <div class="card-body report-chart">
                <h5 class="card-title">{volumeTitle}</h5>
                {volumeLegend}
                {preEscapedToHtml volumeSvg}
            </div>
        </div>
        <div class="row">
            <div class="col-lg-6">
                {chartPanel "report-severity" "Alerts by severity" severitySvg}
                {chartPanel "report-mttr" "Mean time to resolve by severity" mttrSvg}
            </div>
            <div class="col-lg-6">{chartPanel "report-env" breakdownTitle breakdownSvg}</div>
        </div>
    |]
      where
        volumeLegend =
            if null volumeSeverities
                then mempty
                else [hsx|<div class="mb-2" data-testid="report-volume-legend">{forEach volumeSeverities legendBadge}</div>|]
        legendBadge severity =
            [hsx|<span class={"badge severity-badge severity-" <> severity <> " me-1"}>{severity}</span>|]

chartPanel :: Text -> Text -> Text -> Html
chartPanel testId title svg =
    [hsx|
    <div class="card mb-4" data-testid={testId}>
        <div class="card-body report-chart">
            <h5 class="card-title">{title}</h5>
            {preEscapedToHtml svg}
        </div>
    </div>
|]

bucketOption :: Text -> Text -> Html
bucketOption selected value =
    let label = case value of
            "" -> "Auto" :: Text
            "hour" -> "Per hour"
            "day" -> "Per day"
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
    let label = if value == "" then "All environments" else value
     in if Just value == selected || (value == "" && isNothing selected)
            then [hsx|<option value={value} selected="selected">{label}</option>|]
            else [hsx|<option value={value}>{label}</option>|]
