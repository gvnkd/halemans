module Web.View.Dashboard.Index where
import Web.View.Prelude
import Web.View.Fragments (RollupCard (..), rollupCardHtml)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import IHP.TypedSql.RowType (SqlRow)
import IHP.QueryBuilder (orderByAsc)
import qualified IHP.QueryBuilder as QB (query)
import qualified IHP.Fetch as Fetch (fetch)
import qualified Data.Aeson as Aeson
import qualified Data.List as List

data EnvCard = EnvCard
    { cardEnvName :: Maybe Text
    , cardEnvironment :: Maybe Environment
    , cardFiring :: Int64
    , cardAcked :: Int64
    , cardResolved :: Int64
    , cardStalled :: Int64
    , cardSuppressed :: Int64
    , cardWorstSeverity :: Maybe Text
    , cardHourly :: [(UTCTime, Int64)]
    }

-- | Aggregate non-closed alerts into per-environment cards (shared by the
-- dashboard controller and the websocket broadcaster). Cards are keyed by
-- the EFFECTIVE env (a materialized facet named "env" overrides the raw
-- column), so alerts follow their field-mapping override; the inventory row
-- is attached only when an environment with that name exists.
computeEnvCards :: (?modelContext :: ModelContext) => IO ([EnvCard], Maybe EnvCard)
computeEnvCards = do
    counts <- sqlQueryTyped [typedSql|
        SELECT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env_name, a.status, a.severity, count(*), a.suppressed
        FROM alerts a
        WHERE a.status <> 'closed'
        GROUP BY coalesce(nullif(a.facets ->> 'env', ''), a.env), a.status, a.severity, a.suppressed
    |]
    hourly <- sqlQueryTyped [typedSql|
        SELECT coalesce(nullif(a.facets ->> 'env', ''), a.env) AS env_name, date_trunc('hour', a.created_at) AS hour, count(*)
        FROM alerts a
        WHERE a.created_at > now() - interval '24 hours'
        GROUP BY coalesce(nullif(a.facets ->> 'env', ''), a.env), date_trunc('hour', a.created_at)
        ORDER BY hour
    |]
    environments <- QB.query @Environment |> orderByAsc #name |> Fetch.fetch
    let lookupEnv name = find (\environment -> Just environment.name == name) environments
        cardNames = List.sort (List.nub (map (Just . (.name)) environments ++ map (get #env_name) counts))
        cards = [buildCard counts hourly name (lookupEnv name) | name <- cardNames, isJust name]
        unassigned = buildCard counts hourly Nothing Nothing
    pure (cards, if cardTotal unassigned > 0 then Just unassigned else Nothing)

type CountRow = SqlRow '[ '("env_name", Maybe Text), '("status", Text), '("severity", Text), '("count", Int64), '("suppressed", Bool)]
type HourRow = SqlRow '[ '("env_name", Maybe Text), '("hour", Maybe UTCTime), '("count", Int64)]

buildCard :: [CountRow] -> [HourRow] -> Maybe Text -> Maybe Environment -> EnvCard
buildCard counts hourly envName environment =
    let relevant = filter (\row -> get #env_name row == envName) counts
        countFor status = sum [get #count row | row <- relevant, get #status row == status]
        suppressedCount = sum [get #count row | row <- relevant, get #suppressed row]
        severities = nub (map (get #severity) relevant)
        hourlyBuckets = [(hour, get #count row) | row <- hourly, get #env_name row == envName, Just hour <- [get #hour row]]
    in EnvCard
        { cardEnvName = envName
        , cardEnvironment = environment
        , cardFiring = countFor "firing"
        , cardAcked = countFor "ack"
        , cardResolved = countFor "resolved"
        , cardStalled = countFor "stalled"
        , cardSuppressed = suppressedCount
        , cardWorstSeverity = cardWorst severities
        , cardHourly = hourlyBuckets
        }

cardTotal :: EnvCard -> Int64
cardTotal card = card.cardFiring + card.cardAcked + card.cardResolved + card.cardStalled

cardSeverityRank :: Text -> Int
cardSeverityRank = \case
    "critical" -> 0
    "high" -> 1
    "warning" -> 2
    "info" -> 3
    _ -> 4

cardWorst :: [Text] -> Maybe Text
cardWorst [] = Nothing
cardWorst severities = Just (minimumBy (comparing cardSeverityRank) severities)

data IndexView = IndexView
    { cards :: [EnvCard]
    , unassigned :: Maybe EnvCard
    , teamDefault :: Maybe Aeson.Value
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Overview</h1>
        {teamDefaultBanner}
        <div id="env-cards" data-testid="env-cards" data-live-scope="dashboard">
            {forEach cards renderCard}
            {forEach unassigned renderCard}
        </div>
    |]
        where
            teamDefaultBanner = case teamDefault of
                Nothing -> mempty
                Just config -> teamBanner config

-- Team default fallback (milestone_3.md §7): a user with no dashboards sees
-- the team's default_dashboard_config offer and can copy it in one click.
teamBanner :: Aeson.Value -> Html
teamBanner config = [hsx|
    <div class="alert alert-info d-flex justify-content-between align-items-center" data-testid="team-default-banner">
        <span>Your team has a default dashboard template.</span>
        <form method="POST" action={CreateDashboardAction}>
            <input type="hidden" name="name" value="Team default"/>
            <input type="hidden" name="config" value={cs (Aeson.encode config) :: Text}/>
            <button type="submit" class="btn btn-sm btn-primary" data-testid="save-team-default">Save as my dashboard</button>
        </form>
    </div>
|]

renderCard :: EnvCard -> Html
renderCard card = [hsx|
    <div class="env-card-tile mb-3" id={cardDomId card} data-testid="env-card">
        {rollupCardHtml rollup}
    </div>
|]
    where
        rollup = RollupCard
            { rcTitle = cardLink card
            , rcWorstSeverity = card.cardWorstSeverity
            , rcFiring = fromIntegral card.cardFiring
            , rcAcked = fromIntegral card.cardAcked
            , rcResolved = fromIntegral card.cardResolved
            , rcStalled = fromIntegral card.cardStalled
            , rcSuppressed = fromIntegral card.cardSuppressed
            , rcHourly = map (fmap fromIntegral) card.cardHourly
            , rcLink = Nothing
            , rcSize = Nothing
            }

cardLink :: EnvCard -> Html
cardLink card = case (card.cardEnvironment, card.cardEnvName) of
    (Just environment, _) -> [hsx|<a href={ShowEnvironmentAction environment.name}>{environment.name}</a>|]
    (Nothing, Just name) -> [hsx|<span>{name}</span>|]
    (Nothing, Nothing) -> [hsx|<span>unassigned</span>|]

cardDomId :: EnvCard -> Text
cardDomId card = "env-card-" <> fromMaybe "unassigned" card.cardEnvName
