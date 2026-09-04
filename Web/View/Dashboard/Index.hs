module Web.View.Dashboard.Index where
import Web.View.Prelude
import IHP.TypedSql (sqlQueryTyped, typedSql)
import IHP.TypedSql.RowType (SqlRow)
import IHP.QueryBuilder (orderByAsc)
import qualified IHP.QueryBuilder as QB (query)
import qualified IHP.Fetch as Fetch (fetch)

data EnvCard = EnvCard
    { cardEnvironment :: Maybe Environment
    , cardFiring :: Int64
    , cardAcked :: Int64
    , cardResolved :: Int64
    , cardSuppressed :: Int64
    , cardWorstSeverity :: Maybe Text
    , cardHourly :: [(UTCTime, Int64)]
    }

-- | Aggregate non-closed alerts into per-environment cards (shared by the
-- dashboard controller and the websocket broadcaster).
computeEnvCards :: (?modelContext :: ModelContext) => IO ([EnvCard], Maybe EnvCard)
computeEnvCards = do
    counts <- sqlQueryTyped [typedSql|
        SELECT a.environment_id, a.status, a.severity, count(*), a.suppressed
        FROM alerts a
        WHERE a.status <> 'closed'
        GROUP BY a.environment_id, a.status, a.severity, a.suppressed
    |]
    hourly <- sqlQueryTyped [typedSql|
        SELECT a.environment_id, date_trunc('hour', a.created_at) AS hour, count(*)
        FROM alerts a
        WHERE a.created_at > now() - interval '24 hours'
        GROUP BY a.environment_id, date_trunc('hour', a.created_at)
        ORDER BY hour
    |]
    environments <- QB.query @Environment |> orderByAsc #name |> Fetch.fetch
    let cards = map (buildCard counts hourly . Just) environments
        unassigned = buildCard counts hourly Nothing
    pure (cards, if cardTotal unassigned > 0 then Just unassigned else Nothing)

type CountRow = SqlRow '[ '("environment_id", Maybe (Id Environment)), '("status", Text), '("severity", Text), '("count", Int64), '("suppressed", Bool)]
type HourRow = SqlRow '[ '("environment_id", Maybe (Id Environment)), '("hour", Maybe UTCTime), '("count", Int64)]

buildCard :: [CountRow] -> [HourRow] -> Maybe Environment -> EnvCard
buildCard counts hourly environment =
    let envId = get #id <$> environment
        relevant = filter (\row -> get #environment_id row == envId) counts
        countFor status = sum [get #count row | row <- relevant, get #status row == status]
        suppressedCount = sum [get #count row | row <- relevant, get #suppressed row]
        severities = nub (map (get #severity) relevant)
        hourlyBuckets = [(hour, get #count row) | row <- hourly, get #environment_id row == envId, Just hour <- [get #hour row]]
    in EnvCard
        { cardEnvironment = environment
        , cardFiring = countFor "firing"
        , cardAcked = countFor "ack"
        , cardResolved = countFor "resolved"
        , cardSuppressed = suppressedCount
        , cardWorstSeverity = worstSeverity severities
        , cardHourly = hourlyBuckets
        }

cardTotal :: EnvCard -> Int64
cardTotal card = card.cardFiring + card.cardAcked + card.cardResolved

severityRank :: Text -> Int
severityRank = \case
    "critical" -> 0
    "high" -> 1
    "warning" -> 2
    "info" -> 3
    _ -> 4

worstSeverity :: [Text] -> Maybe Text
worstSeverity [] = Nothing
worstSeverity severities = Just (minimumBy (comparing severityRank) severities)

data IndexView = IndexView { cards :: [EnvCard], unassigned :: Maybe EnvCard }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Overview</h1>
        <div class="row" data-testid="env-cards" data-live-scope="dashboard">
            {forEach cards renderCard}
            {forEach unassigned renderCard}
        </div>
    |]

renderCard :: EnvCard -> Html
renderCard card = [hsx|
    <div class="col-md-4 mb-3" id={cardDomId card} data-testid="env-card">
        <div class={"card env-card " <> severityClass card.cardWorstSeverity}>
            <div class="card-body">
                <h5 class="card-title">
                    {cardLink card}
                    {severityBadge card.cardWorstSeverity}
                </h5>
                <div class="env-counts">
                    <span class="count status-firing" data-testid="count-firing">{card.cardFiring} firing</span>
                    <span class="count status-ack">{card.cardAcked} ack</span>
                    <span class="count status-resolved">{card.cardResolved} resolved</span>
                    {suppressedBadge card.cardSuppressed}
                </div>
                <div class="env-hourly" title="alerts per hour (last 24h)">
                    {forEach card.cardHourly renderHourBucket}
                </div>
            </div>
        </div>
    </div>
|]

cardLink :: EnvCard -> Html
cardLink card = case card.cardEnvironment of
    Just environment -> [hsx|<a href={ShowEnvironmentAction environment.name}>{environment.name}</a>|]
    Nothing -> [hsx|<span>unassigned</span>|]

severityBadge :: Maybe Text -> Html
severityBadge Nothing = mempty
severityBadge (Just severity) = [hsx|<span class={"badge severity-badge severity-" <> severity}>{severity}</span>|]

suppressedBadge :: Int64 -> Html
suppressedBadge count
    | count > 0 = [hsx|<span class="count status-suppressed" data-testid="count-suppressed" title="muted by blackout">{count} suppressed</span>|]
    | otherwise = mempty

renderHourBucket :: (UTCTime, Int64) -> Html
renderHourBucket (hour, count) = [hsx|
    <span class="hourly-bucket">
        <span class="hourly-count">{count}</span>
        <span class="hourly-hour">{formatTime defaultTimeLocale "%H:%M" hour}</span>
    </span>
|]

cardDomId :: EnvCard -> Text
cardDomId card = "env-card-" <> maybe "unassigned" (\environment -> environment.name) card.cardEnvironment

severityClass :: Maybe Text -> Text
severityClass = \case
    Just severity -> "env-card-" <> severity
    Nothing -> "env-card-ok"
