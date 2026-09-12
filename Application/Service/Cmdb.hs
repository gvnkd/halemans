module Application.Service.Cmdb (
    ConfPage (..),
    CmdbConfig (..),
    apiUrl,
    cqlForSubject,
    spaceClause,
    pickBestPage,
    excerptFromHtml,
    excerptBudget,
    isFresh,
    positiveTtlSeconds,
    negativeTtlSeconds,
    Subject (..),
    subjectOf,
    subjectTerm,
    fetchCached,
    upsertEntry,
    sourceSpaceOverride,
    confluenceSearch,
    connectionOk,
) where

import qualified Application.Service.Http as Http
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (Value, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import Generated.Types hiding (CmdbConfig)
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import qualified Network.Wreq as Wreq

-- Read-only Confluence CMDB client + TTL cache (design_docs/milestone_3.md
-- §4). Positive cache TTL 6h, negative 30m; stale rows are served while a
-- refresh attempt runs (the card never blocks on Confluence).

excerptBudget :: Int
excerptBudget = 2000

positiveTtlSeconds, negativeTtlSeconds :: NominalDiffTime
positiveTtlSeconds = 6 * 3600
negativeTtlSeconds = 30 * 60

data CmdbConfig = CmdbConfig
    { baseUrl :: Text
    , token :: Text
    , space :: Text
    , spaces :: [Text]
    }
    deriving (Eq, Show)

apiUrl :: CmdbConfig -> Text -> Text
apiUrl config path = Text.dropWhileEnd (== '/') config.baseUrl <> path

-- Per-source scope override: a "cmdbSpaces" array on source.config REPLACES
-- the connection's space list for that source's alerts. The legacy scalar
-- "cmdbSpace" key is honoured when the array is absent.
sourceSpaceOverride :: Source -> [Text]
sourceSpaceOverride source = configStrings "cmdbSpaces" "cmdbSpace" source.config

configStrings :: Text -> Text -> Value -> [Text]
configStrings key legacyKey value = case value of
    Aeson.Object o -> case KeyMap.lookup (Key.fromText key) o of
        Just raw -> fromMaybe [] (parseMaybe Aeson.parseJSON raw)
        Nothing -> maybe [] pure (parseMaybe Aeson.parseJSON =<< KeyMap.lookup (Key.fromText legacyKey) o)
    _ -> []

data ConfPage = ConfPage
    { pageId :: Text
    , pageTitle :: Text
    , pageBodyHtml :: Text
    , pageWebui :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ConfPage where
    parseJSON = Aeson.withObject "ConfPage" \o -> do
        pageId <- o .: "id"
        pageTitle <- o .: "title"
        body <- o .:? "body"
        pageBodyHtml <- case body of
            Just b ->
                b .:? "view" >>= \case
                    Just view -> view .:? "value" .!= ""
                    Nothing -> pure ""
            Nothing -> pure ""
        links <- o .:? "_links"
        pageWebui <- case links of
            Just l -> l .:? "webui" .!= ""
            Nothing -> pure ""
        pure ConfPage{..}

confluenceSearch :: CmdbConfig -> Text -> IO (Either Text [ConfPage])
confluenceSearch config cql = do
    let opts =
            Wreq.defaults
                & Wreq.header "Authorization" .~ ["Bearer " <> cs config.token]
                & Wreq.param "cql" .~ [cql]
    result <- try (Http.getFollowing opts (cs (apiUrl config "/rest/api/content/search")))
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case Aeson.eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded -> do
                let pages = parseMaybe (Aeson.withObject "search" (.: "results")) decoded
                pure (maybe (Left "confluence search: no results field") Right pages)

connectionOk :: CmdbConfig -> IO (Either Text ())
connectionOk config = either Left (const (Right ())) <$> confluenceSearch config "type = page"

-- Multi-space CQL (milestone 10): several spaces from cmdb_configs are OR-ed
-- via `space in (...)`; an empty list drops the space clause entirely.
cqlForSubject :: [Text] -> Text -> Text
cqlForSubject spaces term =
    spaceClause spaces <> "text ~ \"" <> term <> "\" AND type = page"

spaceClause :: [Text] -> Text
spaceClause [] = ""
spaceClause [space] = "space = \"" <> space <> "\" AND "
spaceClause spaces =
    "space in (" <> Text.intercalate ", " (map quote spaces) <> ") AND "
  where
    quote space = "\"" <> space <> "\""

-- Exact title match wins; otherwise the first hit (milestone_3.md §4).
pickBestPage :: Text -> [ConfPage] -> Maybe ConfPage
pickBestPage _ [] = Nothing
pickBestPage term pages@(page : _) =
    case filter (\candidate -> Text.toLower candidate.pageTitle == Text.toLower term) pages of
        (exact : _) -> Just exact
        [] -> Just page

excerptFromHtml :: Int -> Text -> Text
excerptFromHtml budget html =
    let stripped = Text.unwords (Text.words (stripHtmlTags html))
     in if Text.length stripped > budget
            then Text.take budget stripped <> "…"
            else stripped

stripHtmlTags :: Text -> Text
stripHtmlTags input = case Text.breakOn "<" input of
    (before, rest)
        | Text.null rest -> before
        | otherwise -> case Text.breakOn ">" (Text.drop 1 rest) of
            (_, after)
                | Text.null after -> before
                | otherwise -> before <> " " <> stripHtmlTags (Text.drop 1 after)

isFresh :: UTCTime -> UTCTime -> NominalDiffTime -> Bool
isFresh now fetchedAt ttl = diffUTCTime now fetchedAt < ttl

data Subject = SubjectHost (Id Host) Text | SubjectService (Id Service) Text

subjectOf :: Alert -> Maybe Subject
subjectOf alert = case (alert.hostId, alert.host) of
    (Just hostId, Just fqdn) -> Just (SubjectHost hostId fqdn)
    _ -> case (alert.serviceId, alert.service) of
        (Just serviceId, Just name) -> Just (SubjectService serviceId name)
        _ -> Nothing

fetchCached :: (?modelContext :: ModelContext) => Subject -> IO (Maybe CmdbEntry)
fetchCached (SubjectHost hostId _) =
    query @CmdbEntry
        |> filterWhere (#hostId, Just hostId)
        |> fetchOneOrNothing
fetchCached (SubjectService serviceId _) =
    query @CmdbEntry
        |> filterWhere (#serviceId, Just serviceId)
        |> fetchOneOrNothing

subjectTerm :: Subject -> Text
subjectTerm (SubjectHost _ fqdn) = fqdn
subjectTerm (SubjectService _ name) = name

upsertEntry :: (?modelContext :: ModelContext) => Subject -> Maybe ConfPage -> CmdbConfig -> IO CmdbEntry
upsertEntry subject page config = do
    now <- getCurrentTime
    existing <- fetchCached subject
    let applyFields record =
            record
                |> set #pageId (page <&> (.pageId))
                |> set #title (maybe "" (.pageTitle) page)
                |> set #excerpt (maybe "" (excerptFromHtml excerptBudget . (.pageBodyHtml)) page)
                |> set #url (maybe "" (\p -> apiUrl config p.pageWebui) page)
                |> set #fetchedAt now
    entry <- case existing of
        Just entry -> updateRecord (applyFields entry)
        Nothing -> createRecord (applyFields (subjectRecord (newRecord @CmdbEntry)))
    forM_ page \found -> case subject of
        SubjectHost hostId _ -> do
            host <- fetch hostId
            _ <- host |> set #cmdbPageId (Just found.pageId) |> updateRecord
            pure ()
        SubjectService serviceId _ -> do
            service <- fetch serviceId
            _ <- service |> set #cmdbPageId (Just found.pageId) |> updateRecord
            pure ()
    pure entry
  where
    subjectRecord record = case subject of
        SubjectHost hostId _ -> record |> set #hostId (Just hostId)
        SubjectService serviceId _ -> record |> set #serviceId (Just serviceId)

-- Cache-first alert lookup (lookupForAlert/refreshForAlert/resolve) lives in
-- Application.Service.Cmdb.DbConfig — it needs DB config resolution, which
-- imports this module.
