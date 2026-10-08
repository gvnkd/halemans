module Application.Connector.GrafanaMetrics (
    MetricSeries (..),
    ruleUidFromSourceUrl,
    ruleQueryGet,
    ruleQueryFromRule,
    dsQueryRange,
    seriesFromResponse,
    framesFromChunks,
    datasourceTypeGet,
    buildExploreUrl,
) where

import Application.Connector.Grafana (elementStep, manager, runStep)
import Application.Service.Http (getFollowing, postFollowingStream)
import Control.Exception (SomeException, displayException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as Vector
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.URI (urlEncode)
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
-- drives intervalMs. The response is parsed incrementally, one data frame
-- at a time (framesFromChunks over a streaming body): a datasource that
-- ignores maxDataPoints can return a huge frame array, and the old
-- full-response decode OOMed the worker on such sources — the same failure
-- class as the alertmanager listing fix (alertsFold).
dsQueryRange :: Text -> Text -> Text -> Text -> UTCTime -> UTCTime -> Int -> IO (Either Text [MetricSeries])
dsQueryRange baseUrl token datasourceUid expr from to maxPoints = do
    let windowMs = max 1000 (utcToMs to - utcToMs from)
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
    outcome <-
        try
            ( postFollowingStream
                manager
                (cs (baseUrl <> "/api/ds/query"))
                [("Authorization", "Bearer " <> cs token)]
                body
                \response -> do
                    let bodyReader = HTTP.responseBody response
                    opened <- runStep bodyReader openFrames BS.empty
                    case opened of
                        Left err -> pure (Left err)
                        Right ((), leftover) -> foldStream bodyReader leftover []
            ) ::
            IO (Either SomeException (Either Text [MetricSeries]))
    pure case outcome of
        Left err -> Left (cs (displayException err))
        Right (Left err) -> Left err
        Right (Right series)
            | null series -> Left "ds/query: no data frames"
            | otherwise -> Right series
  where
    foldStream bodyReader leftover acc = do
        stepped <- runStep bodyReader elementStep leftover
        case stepped of
            Left err -> pure (Left err)
            Right (Nothing, _) -> pure (Right (reverse acc))
            Right (Just (value, more), rest) -> do
                let acc' = case frameToSeries maxPoints value of
                        Just series -> series : acc
                        Nothing -> acc
                if more
                    then foldStream bodyReader rest acc'
                    else pure (Right (reverse acc'))

-- | Incremental parser entry: consume {"results": {"<refId>": ... up to the
-- frames array's opening '['. Object keys are located generically (the key
-- order of grafana's response is not contractual), everything before
-- "frames" is skipped one value at a time.
openFrames :: Atto.Parser ()
openFrames = do
    Atto.skipSpace
    _ <- Atto.char '{'
    foundResults <- objectFind "results"
    case foundResults of
        Nothing -> fail "ds/query: missing results"
        Just () -> do
            Atto.skipSpace
            _ <- Atto.char '{'
            _ <- AesonParser.value -- the refId key ("A")
            Atto.skipSpace
            _ <- Atto.char ':'
            Atto.skipSpace
            _ <- Atto.char '{' -- the result object's opening brace
            foundFrames <- objectFind "frames"
            case foundFrames of
                Nothing -> fail "ds/query: missing frames"
                Just () -> Atto.skipSpace >> Atto.char '[' >> pure ()

-- | Loop over one JSON object's keys: consume the value of every key other
-- than `wanted` (returning Nothing when the object ends first).
objectFind :: Text -> Atto.Parser (Maybe ())
objectFind wanted = go
  where
    go = do
        Atto.skipSpace
        closer <- Atto.peekChar'
        if closer == '}'
            then Atto.char '}' >> pure Nothing
            else do
                key <- AesonParser.value
                Atto.skipSpace
                _ <- Atto.char ':'
                case key of
                    String k | k == wanted -> pure (Just ())
                    _ -> do
                        _ <- AesonParser.value
                        Atto.skipSpace
                        separator <- Atto.satisfy (\word -> word == ',' || word == '}')
                        if separator == '}'
                            then pure Nothing
                            else go

-- | Pure chunk-driver over the same incremental parser the socket path
-- uses: feeds the parser one chunk at a time without ever materializing
-- the whole response as one aeson value. Test entry point.
framesFromChunks :: Int -> [BS.ByteString] -> Either Text [MetricSeries]
framesFromChunks maxPoints chunks = do
    ((), rest) <- runParserChunks chunks openFrames
    go rest []
  where
    go input acc = do
        (stepped, rest) <- runParserChunks input elementStep
        case stepped of
            Nothing -> Right (reverse acc)
            Just (value, more) -> do
                let acc' = case frameToSeries maxPoints value of
                        Just series -> series : acc
                        Nothing -> acc
                if more
                    then go rest acc'
                    else Right (reverse acc')

-- | Runs one parser step against an in-memory chunk list; returns the
-- result with the unconsumed chunks.
runParserChunks :: [BS.ByteString] -> Atto.Parser step -> Either Text (step, [BS.ByteString])
runParserChunks chunks parser = go chunks (Atto.parse parser BS.empty)
  where
    go [] (Atto.Done leftover result) = Right (result, nonNull leftover)
    go [] (Atto.Partial continue) = finish (continue BS.empty) []
    go (chunk : rest) (Atto.Partial continue) = go rest (continue chunk)
    go _ (Atto.Fail _ _ err) = Left (cs err)
    go (chunk : rest) (Atto.Done leftover result) = Right (result, nonNull leftover ++ (chunk : rest))
    finish (Atto.Done leftover result) rest = Right (result, nonNull leftover ++ rest)
    finish (Atto.Partial _) _ = Left "ds/query: unexpected end of input"
    finish (Atto.Fail _ _ err) _ = Left (cs err)
    nonNull leftover = [leftover | not (BS.null leftover)]

utcToMs :: UTCTime -> Integer
utcToMs t = floor (realToFrac (t `diffUTCTime` posixEpoch) * 1000 :: Double)
  where
    posixEpoch = posixSecondsToUTCTime 0

-- | Best-effort datasource type lookup, for the Explore link's datasource
-- ref. Nothing when the lookup fails — Grafana resolves uid-only refs.
datasourceTypeGet :: Text -> Text -> Text -> IO (Maybe Text)
datasourceTypeGet baseUrl token uid = do
    outcome <- try do
        let opts = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs token]
        response <- getFollowing opts (cs (baseUrl <> "/api/datasources/uid/" <> uid))
        pure (eitherDecode (response ^. Wreq.responseBody))
    pure case outcome of
        Left (_ :: SomeException) -> Nothing
        Right (Left _) -> Nothing
        Right (Right value) -> lookupKey "type" value >>= asString

-- | Grafana Explore URL preloaded with the alert rule's query, so the card
-- links straight to the full metrics explorer. datasourceType is included
-- when known (see datasourceTypeGet).
buildExploreUrl :: Text -> Text -> Maybe Text -> Text -> Text
buildExploreUrl baseUrl datasourceUid datasourceType expr =
    baseUrl <> "/explore?orgId=1&schemaVersion=1&panes=" <> encodedPanes
  where
    encodedPanes =
        cs (urlEncode True (cs (Aeson.encode panes)))
    panes =
        Aeson.object
            [ "pane-1"
                .= Aeson.object
                    [ "datasource" .= datasourceRef
                    , "queries"
                        .= [ Aeson.object
                                [ "refId" .= ("A" :: Text)
                                , "datasource" .= datasourceRef
                                , "expr" .= expr
                                , "range" .= True
                                ]
                           ]
                    , "range"
                        .= Aeson.object
                            [ "from" .= ("now-3h" :: Text)
                            , "to" .= ("now" :: Text)
                            ]
                    ]
            ]
    datasourceRef = case datasourceType of
        Just t -> Aeson.object ["type" .= t, "uid" .= datasourceUid]
        Nothing -> Aeson.object ["uid" .= datasourceUid]

-- Response decoding: {"results": {"A": {"status": 200, "frames": [...]}}}.
-- Each frame carries schema.fields [Time, Value] and data.values as two
-- parallel arrays (epoch-ms timestamps, sample values); multiple frames are
-- multiple series. maxPoints caps each decoded series: datasources may ignore
-- maxDataPoints (or a recording rule may pre-aggregate densely), and an
-- uncapped frame would blow the heap on a wide window.
seriesFromResponse :: Int -> Value -> Either Text [MetricSeries]
seriesFromResponse maxPoints value = do
    results <- maybe (Left "ds/query: missing results") Right (lookupKey "results" value)
    a <- maybe (Left "ds/query: missing result A") Right (lookupKey "A" results)
    frames <- maybe (Left "ds/query: missing frames") Right (lookupKey "frames" a >>= asArray)
    let series = mapMaybe (frameToSeries maxPoints) (Vector.toList frames)
    if null series then Left "ds/query: no data frames" else Right series

frameToSeries :: Int -> Value -> Maybe MetricSeries
frameToSeries maxPoints frame = do
    schema <- lookupKey "schema" frame
    fields <- lookupKey "fields" schema >>= asArray
    valueField <- fields Vector.!? 1
    let name = frameDisplayName valueField
    data_ <- lookupKey "data" frame
    values <- lookupKey "values" data_ >>= asArray
    times <- values Vector.!? 0 >>= asArray
    numbers <- values Vector.!? 1 >>= asArray
    let points = sampleFramePoints maxPoints times numbers
    pure (MetricSeries name points)

-- Series title for the legend: real grafana range frames name the value
-- field "Value" and carry the identity in labels ({"__name__": "up",
-- "instance": "..."}) or config.displayNameFromDS; the mock names the field
-- directly. Precedence mirrors what the Grafana UI shows.
frameDisplayName :: Value -> Text
frameDisplayName valueField = case configName of
    Just n -> n
    Nothing -> case labelsName of
        Just n -> n
        Nothing -> fromMaybe "Value" (lookupKey "name" valueField >>= asString)
  where
    config = lookupKey "config" valueField
    configName =
        (config >>= lookupKey "displayNameFromDS" >>= asString)
            `orElse` (config >>= lookupKey "displayName" >>= asString)
    labelsName = do
        labelsObj <- lookupKey "labels" valueField
        pairs <- case labelsObj of
            Object o ->
                Just
                    [ (Key.toText k, v)
                    | (k, String v) <- KeyMap.toList o
                    ]
            _ -> Nothing
        let metricName = lookup "__name__" pairs
            labelPairs = [(k, v) | (k, v) <- pairs, k /= "__name__"]
            inner = Text.intercalate ", " [k <> "=\"" <> v <> "\"" | (k, v) <- sortOn fst labelPairs]
        pure case metricName of
            Just n | Text.null inner -> n
            Just n -> n <> "{" <> inner <> "}"
            Nothing | Text.null inner -> "Value"
            Nothing -> "{" <> inner <> "}"
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | Stride-thin a series to at most maxPoints samples (first sample kept,
-- then every stride-th), computed INDEX-WISE over the parallel vectors so a
-- dense frame never materializes its full sample list — only the surviving
-- samples are built. Defense against datasources ignoring maxDataPoints.
-- Same selection as the old list-based thinPoints.
sampleFramePoints :: Int -> Vector.Vector Value -> Vector.Vector Value -> [(UTCTime, Double)]
sampleFramePoints maxPoints times numbers
    | maxPoints <= 0 = allPoints
    | n <= maxPoints = allPoints
    | otherwise = mapMaybe pointAt [0, stride .. n - 1]
  where
    pairs = Vector.zip times numbers
    n = Vector.length pairs
    stride = (n + maxPoints - 1) `div` maxPoints
    allPoints = mapMaybe pointAt [0 .. n - 1]
    pointAt index =
        let (msValue, numberValue) = pairs Vector.! index
         in case (asMs msValue, asDouble numberValue) of
                (Just ms, Just value) ->
                    Just (posixSecondsToUTCTime (realToFrac ms / 1000), value)
                _ -> Nothing

asMs :: Value -> Maybe Integer
asMs (Number numberValue) = either (const Nothing) Just (floatingOrInteger numberValue :: Either Double Integer)
asMs _ = Nothing

asDouble :: Value -> Maybe Double
asDouble (Number numberValue) = Just (realToFrac numberValue)
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
