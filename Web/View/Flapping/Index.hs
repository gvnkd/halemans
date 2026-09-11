module Web.View.Flapping.Index where
import Web.View.Prelude
import Application.Service.Flapping (FlapReport (..), FlapSubject (..))
import Web.View.Fragments (severityBadgeHtml)

data IndexView = IndexView
    { reports :: [FlapReport]
    , windowHours :: Int
    , minFlaps :: Int
    , maxGapSeconds :: Int
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Flapping alerts</h1>
        <form method="GET" action={FlappingAction} class="row g-2 align-items-end mb-4" data-testid="flapping-form">
            <div class="col-auto">
                <label class="form-label">Window</label>
                <select name="windowHours" class="form-select" data-testid="flapping-window">
                    {forEach [6, 24, 168, 720] (windowOption windowHours)}
                </select>
            </div>
            <div class="col-auto">
                <label class="form-label">Min flaps</label>
                <input type="number" name="minFlaps" class="form-control maxw-400" min="1" value={show minFlaps :: Text} data-testid="flapping-min-flaps"/>
            </div>
            <div class="col-auto">
                <label class="form-label">Max gap (s)</label>
                <input type="number" name="maxGapSeconds" class="form-control maxw-400" min="60" value={show maxGapSeconds :: Text} data-testid="flapping-max-gap"/>
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-primary" data-testid="flapping-submit">Analyze</button>
            </div>
        </form>
        {resultsTable reports}
    |]

resultsTable :: [FlapReport] -> Html
resultsTable [] = [hsx|<p class="text-muted" data-testid="flapping-empty">No flapping alerts in the selected window.</p>|]
resultsTable reports = [hsx|
    <table class="table" data-testid="flapping-table">
        <thead>
            <tr>
                <th>Alert</th>
                <th>Fingerprint</th>
                <th>Severity</th>
                <th>Env</th>
                <th>Host</th>
                <th>Source</th>
                <th>Flaps</th>
                <th>Rate/h</th>
                <th>Median gap</th>
                <th>P90 gap</th>
                <th>MTTR</th>
                <th>Last flap</th>
            </tr>
        </thead>
        <tbody>
            {forEach reports renderReportRow}
        </tbody>
    </table>
|]

renderReportRow :: FlapReport -> Html
renderReportRow report =
    let subject = report.subject
        rate = show (fromIntegral (round (report.flapRatePerHour * 100) :: Int) / (100 :: Double)) :: Text
        flapCount = show report.flapCount :: Text
    in [hsx|
    <tr data-testid="flapping-row">
        <td><a href={pathTo (ShowAlertAction subject.latestAlertId)}>{subject.title}</a></td>
        <td><code>{subject.fingerprint}</code></td>
        <td>{severityBadgeHtml subject.severity Nothing}</td>
        <td>{fromMaybe "" subject.effectiveEnv}</td>
        <td>{fromMaybe "" subject.host}</td>
        <td>{fromMaybe "" subject.sourceName}</td>
        <td>{flapCount}</td>
        <td>{rate}</td>
        <td>{formatSeconds (Just report.medianGapSeconds)}</td>
        <td>{formatSeconds (Just report.p90GapSeconds)}</td>
        <td>{formatSeconds report.mttrSeconds}</td>
        <td>{utcTimeHtml report.lastFlapAt}</td>
    </tr>
|]

windowOption :: Int -> Int -> Html
windowOption selected value =
    let label = case value of
            6 -> "6h" :: Text
            24 -> "24h"
            168 -> "7d"
            720 -> "30d"
            other -> show other <> "h"
        valueText = show value :: Text
    in if value == selected
        then [hsx|<option value={valueText} selected="selected">{label}</option>|]
        else [hsx|<option value={valueText}>{label}</option>|]

formatSeconds :: Maybe Double -> Text
formatSeconds Nothing = "-"
formatSeconds (Just seconds)
    | seconds < 90 = show (round seconds :: Int) <> "s"
    | seconds < 5400 = show (round (seconds / 60) :: Int) <> "m"
    | otherwise = show (round (seconds / 3600) :: Int) <> "h"
