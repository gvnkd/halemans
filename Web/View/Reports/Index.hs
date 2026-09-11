module Web.View.Reports.Index where
import Web.View.Prelude

data IndexView = IndexView
    { windowHours :: Int
    , severitySvg :: Text
    , envSvg :: Text
    , volumeSvg :: Text
    , mttrSvg :: Text
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Reports</h1>
        <form method="GET" action={ReportsAction} class="row g-2 align-items-end mb-4" data-testid="reports-form">
            <div class="col-auto">
                <label class="form-label">Window</label>
                <select name="windowHours" class="form-select" data-testid="reports-window">
                    {forEach [24, 168, 720] (windowOption windowHours)}
                </select>
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-primary" data-testid="reports-submit">Render</button>
            </div>
        </form>
        {chartPanel "report-volume" "Alert volume per day" volumeSvg}
        <div class="row">
            <div class="col-lg-6">{chartPanel "report-severity" "Alerts by severity" severitySvg}</div>
            <div class="col-lg-6">{chartPanel "report-env" "Alerts by environment" envSvg}</div>
        </div>
        {chartPanel "report-mttr" "Mean time to resolve by severity (minutes)" mttrSvg}
    |]

chartPanel :: Text -> Text -> Text -> Html
chartPanel testId title svg = [hsx|
    <div class="card mb-4" data-testid={testId}>
        <div class="card-body">
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
