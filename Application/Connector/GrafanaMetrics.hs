module Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    ruleUidFromSourceUrl,
    ruleQueryGet,
    ruleQueryFromRule,
    dsQueryRange,
    seriesFromResponse,
) where

import Application.Service.Http (getFollowing, postFollowing)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as Vector
import IHP.Prelude
import qualified Network.Wreq as Wreq

-- | Metrics fetch for grafana sources (alert detail chart widget). The alert
-- rule definition is fetched from the provisioning API and its first
-- prometheus-style query re-run as a range query via /api/ds/query, so any
-- datasource Grafana itself can reach (victoriametrics, prometheus, ...)
-- works without per-alert config. Log datasources (clickhouse/loki) have no
-- numeric series and are skipped.
data MetricSeries = MetricSeries
    { seriesName :: Text
    , seriesPoints :: [(UTCTime, Double)]
    }
    deriving (Eq, Show)

-- | Rule UID from a grafana generatorURL of the form
-- "<base>/alerting/grafana/<uid>/view?...". Nothing for foreign URLs.
ruleUidFromSourceUrl :: Text -> Maybe Text
ruleUidFromSourceUrl url =
    go (Text.splitOn "/" (fst (Text.breakOn "?" url)))
  where
    go (a : b : uid : _) | a == "alerting" && b == "grafana" = Just uid
    go (_ : rest) = go rest
    go [] = Nothing

-- | First prometheus-style query of an alert rule: (datasourceUid, expr).
ruleQueryGet :: Text -> Text -> Text -> IO (Either Text (Text, Text))
ruleQueryGet baseUrl token ruleUid = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
    response <- getFollowing opts (cs (baseUrl <> "/api/v1/provisioning/alert-rules/" <> ruleUid))
    case eitherDecode (response ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right value -> pure (ruleQueryFromRule value)

ruleQueryFromRule :: Value -> Either Text (Text, Text)
ruleQueryFromRule rule = do
    items <- maybe (Left "rule: missing data array") Right (lookupKey "data" rule >>= asArray)
    case mapMaybe queryCandidate (Vector.toList items) of
        (candidate : _) -> Right candidate
        [] -> Left "rule: no query with an expr found"
  where
    queryCandidate item = do
        model <- lookupKey "model" item
        expr <- lookupKey "expr" model >>= asString
        datasourceUid <- case lookupKey "datasourceUid" item >>= asString of
            Just uid -> Just uid
            Nothing -> lookupKey "datasource" model >>= lookupKey "uid" >>= asString
        pure (datasourceUid, expr)

-- | Run a range query through grafana's datasource proxy and decode the
-- frame response into time series. maxPoints caps the sample count and
-- drives intervalMs.
dsQueryRange :: Text -> Text -> Text -> Text -> UTCTime -> UTCTime -> Int -> IO (Either Text [MetricSeries])
dsQueryRange baseUrl token datasourceUid expr from to maxPoints = do
    let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        windowMs = max 1000 (utcToMs to - utcToMs from)
        intervalMs = max 1000 (fromIntegral windowMs `div` max 1 maxPoints :: Int)
        body =
            object
                [ "queries"
                    .= [ object
                            [ "refId" .= ("A" :: Text)
                            , "datasource" .= object ["uid" .= datasourceUid]
                            , "expr" .= expr
                            , "range" .= True
                            , "intervalMs" .= intervalMs
                            , "maxDataPoints" .= maxPoints
                            ]
                       ]
                , "from" .= show (utcToMs from)
                , "to" .= show (utcToMs to)
                ]
    response <- postFollowing opts (cs (baseUrl <> "/api/ds/query")) body
    case eitherDecode (response ^. Wreq.responseBody) of
        Left err -> pure (Left (cs err))
        Right value -> pure (seriesFromResponse value)

utcToMs :: UTCTime -> Integer
utcToMs t = floor (realToFrac (t `diffUTCTime` posixEpoch) * 1000 :: Double)
  where
    posixEpoch = posixSecondsToUTCTime 0

-- Response decoding: {"results": {"A": {"status": 200, "frames": [...]}}}.
-- Each frame carries schema.fields [Time, Value] and data.values as two
-- parallel arrays (epoch-ms timestamps, sample values); multiple frames are
-- multiple series.
seriesFromResponse :: Value -> Either Text [MetricSeries]
seriesFromResponse value = do
    results <- maybe (Left "ds/query: missing results") Right (lookupKey "results" value)
    a <- maybe (Left "ds/query: missing result A") Right (lookupKey "A" results)
    frames <- maybe (Left "ds/query: missing frames") Right (lookupKey "frames" a >>= asArray)
    let series = mapMaybe frameToSeries (Vector.toList frames)
    if null series then Left "ds/query: no data frames" else Right series

frameToSeries :: Value -> Maybe MetricSeries
frameToSeries frame = do
    schema <- lookupKey "schema" frame
    fields <- lookupKey "fields" schema >>= asArray
    valueField <- fields Vector.!? 1
    name <- lookupKey "name" valueField >>= asString
    data_ <- lookupKey "data" frame
    values <- lookupKey "values" data_ >>= asArray
    times <- values Vector.!? 0 >>= asArray
    numbers <- values Vector.!? 1 >>= asArray
    let points =
            [ (posixSecondsToUTCTime (realToFrac ms / 1000), v)
            | (Just ms, Just v) <- zip (map asMs (Vector.toList times)) (map asDouble (Vector.toList numbers))
            ]
    pure (MetricSeries name points)
  where
    asMs (Number n) = either (const Nothing) (Just . fromInteger) (floatingOrInteger n :: Either Double Integer)
    asMs _ = Nothing
    asDouble (Number n) = Just (realToFrac n)
    asDouble _ = Nothing

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupKey _ _ = Nothing

asArray :: Value -> Maybe (Vector.Vector Value)
asArray (Array a) = Just a
asArray _ = Nothing

asString :: Value -> Maybe Text
asString (String t) = Just t
asString _ = Nothing
