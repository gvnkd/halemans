module Web.View.Flapping.Index where

import Application.Service.Flapping (FlapReport (..), FlapSubject (..))
import Network.Wai (Request)
import Web.View.Fragments (emptyStateHtml, pageHeaderHtml, severityBadgeHtml)
import Web.View.Prelude

data IndexView = IndexView
    { reports :: [FlapReport]
    , windowHours :: Int
    , minFlaps :: Int
    , maxGapSeconds :: Int
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        <div>
        {pageHeaderHtml (tr "Flapping alerts") mempty}
        <form method="GET" action={FlappingAction} class="row g-2 align-items-end mb-4" data-testid="flapping-form">
            <div class="col-auto">
                <label class="form-label">{tr "Window"}</label>
                <select name="windowHours" class="select" data-testid="flapping-window">
                    {forEach [6, 24, 168, 720] (windowOption windowHours)}
                </select>
            </div>
            <div class="col-auto">
                <label class="form-label">{tr "Min flaps"}</label>
                <input type="number" name="minFlaps" class="form-control maxw-400" min="1" value={show minFlaps :: Text} data-testid="flapping-min-flaps"/>
            </div>
            <div class="col-auto">
                <label class="form-label">{tr "Max gap (s)"}</label>
                <input type="number" name="maxGapSeconds" class="form-control maxw-400" min="60" value={show maxGapSeconds :: Text} data-testid="flapping-max-gap"/>
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-brand" data-testid="flapping-submit">{tr "Analyze"}</button>
            </div>
        </form>
        {resultsTable reports}
        </div>
    |]

resultsTable :: (?request :: Request) => [FlapReport] -> Html
resultsTable [] = emptyStateHtml "flapping-empty" (tr "No flapping alerts in the selected window.")
resultsTable reports =
    [hsx|
    <table class="table" data-testid="flapping-table">
        <thead>
            <tr>
                <th>{tr "Alert"}</th>
                <th>{tr "Fingerprint"}</th>
                <th>{tr "Severity"}</th>
                <th>{tr "Env"}</th>
                <th>{tr "Host"}</th>
                <th>{tr "Source"}</th>
                <th title={tr "Fire/resolve/refire cycles in qualifying episodes (resolve -> refire gap <= max gap)"}>{tr "Flaps"}</th>
                <th title={tr "Flaps per hour over the analysis window"}>{tr "Rate/h"}</th>
                <th title={tr "Median time between a resolve and the following refire — the usual flapping period"}>{tr "Median gap"}</th>
                <th title={tr "90th percentile of resolve -> refire gaps — worst-case flapping period, outliers excluded"}>{tr "P90 gap"}</th>
                <th title={tr "Mean time from firing to resolved (mean time to resolve) across flapping episodes"}>{tr "MTTR"}</th>
                <th title={tr "Most recent refire of a flapping episode"}>{tr "Last flap"}</th>
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
        rate = formatRate report.flapRatePerHour
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

formatRate :: Double -> Text
formatRate rate
    | cents `mod` 100 == 0 = show whole
    | cents `mod` 10 == 0 = show whole <> "." <> show (frac `div` 10)
    | otherwise = show whole <> "." <> (if frac < 10 then "0" else "") <> show frac
  where
    cents = round (rate * 100) :: Int
    whole = cents `div` 100
    frac = cents `mod` 100

formatSeconds :: Maybe Double -> Text
formatSeconds Nothing = "-"
formatSeconds (Just seconds)
    | seconds < 90 = show (round seconds :: Int) <> "s"
    | seconds < 5400 = show (round (seconds / 60) :: Int) <> "m"
    | otherwise = show (round (seconds / 3600) :: Int) <> "h"
