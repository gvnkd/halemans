module Web.View.Reports.Index where

import Web.View.Fragments (filterMultiSelect)
import Web.View.Prelude

data IndexView = IndexView
    { windowHours :: Int
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
                <select name="windowHours" class="form-select" data-testid="reports-window">
                    {forEach [24, 168, 720] (windowOption windowHours)}
                </select>
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

windowOption :: Int -> Int -> Html
windowOption selected value =
    let label = case value of
            24 -> "24h" :: Text
            168 -> "7d"
            720 -> "30d"
            other -> show other <> "h"
        valueText = show value :: Text
     in if value == selected
            then [hsx|<option value={valueText} selected="selected">{label}</option>|]
            else [hsx|<option value={valueText}>{label}</option>|]

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

envOption :: Maybe Text -> Text -> Html
envOption selected value =
    let label = if value == "" then "All environments" else value
     in if Just value == selected || (value == "" && isNothing selected)
            then [hsx|<option value={value} selected="selected">{label}</option>|]
            else [hsx|<option value={value}>{label}</option>|]
