module Application.Service.Cmdb
( ConfPage (..)
, CmdbConfig (..)
, cmdbConfigFromEnv
, cmdbEnvConfig
, apiUrl
, cqlForSubject
, pickBestPage
, excerptFromHtml
, excerptBudget
, isFresh
, lookupForAlert
, refreshForAlert
, confluenceSearch
, connectionOk
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import Data.Aeson (Value, (.:), (.:?), (.!=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Network.Wreq as Wreq
import qualified Application.Service.Http as Http
import Control.Lens ((&), (^.), (.~))
import Control.Exception (try, SomeException)
import Data.Functor ((<&>))
import System.Environment (lookupEnv)

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
    } deriving (Eq, Show)

cmdbConfigFromEnv :: Source -> IO (Maybe CmdbConfig)
cmdbConfigFromEnv source =
    cmdbEnvConfig (configText "cmdbSpace" source.config |> fromMaybe "DEV")

cmdbEnvConfig :: Text -> IO (Maybe CmdbConfig)
cmdbEnvConfig space = do
    url <- lookupEnv "HALEMANS_CONFLUENCE_URL"
    token <- lookupEnv "CONFLUENCE_TOKEN"
    pure case (url, token) of
        (Just url, Just token) -> Just CmdbConfig { baseUrl = cs url, token = cs token, space }
        _ -> Nothing

apiUrl :: CmdbConfig -> Text -> Text
apiUrl config path = Text.dropWhileEnd (== '/') config.baseUrl <> path

configText :: Text -> Value -> Maybe Text
configText key value = parseMaybe (Aeson.withObject "config" (\o -> o .: Key.fromText key)) value

data ConfPage = ConfPage
    { pageId :: Text
    , pageTitle :: Text
    , pageBodyHtml :: Text
    , pageWebui :: Text
    } deriving (Eq, Show)

instance Aeson.FromJSON ConfPage where
    parseJSON = Aeson.withObject "ConfPage" \o -> do
        pageId <- o .: "id"
        pageTitle <- o .: "title"
        body <- o .:? "body"
        pageBodyHtml <- case body of
            Just b -> b .:? "view" >>= \case
                Just view -> view .:? "value" .!= ""
                Nothing -> pure ""
            Nothing -> pure ""
        links <- o .:? "_links"
        pageWebui <- case links of
            Just l -> l .:? "webui" .!= ""
            Nothing -> pure ""
        pure ConfPage { .. }

confluenceSearch :: CmdbConfig -> Text -> IO (Either Text [ConfPage])
confluenceSearch config cql = do
    let opts = Wreq.defaults
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

cqlForSubject :: Text -> Text -> Text
cqlForSubject space term =
    "space = \"" <> space <> "\" AND text ~ \"" <> term <> "\" AND type = page"

-- Exact title match wins; otherwise the first hit (milestone_3.md §4).
pickBestPage :: Text -> [ConfPage] -> Maybe ConfPage
pickBestPage _ [] = Nothing
pickBestPage term pages@(page:_) =
    case filter (\candidate -> Text.toLower candidate.pageTitle == Text.toLower term) pages of
        (exact:_) -> Just exact
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
fetchCached (SubjectHost hostId _) = query @CmdbEntry
    |> filterWhere (#hostId, Just hostId)
    |> fetchOneOrNothing
fetchCached (SubjectService serviceId _) = query @CmdbEntry
    |> filterWhere (#serviceId, Just serviceId)
    |> fetchOneOrNothing

subjectTerm :: Subject -> Text
subjectTerm (SubjectHost _ fqdn) = fqdn
subjectTerm (SubjectService _ name) = name

upsertEntry :: (?modelContext :: ModelContext) => Subject -> Maybe ConfPage -> CmdbConfig -> IO CmdbEntry
upsertEntry subject page config = do
    now <- getCurrentTime
    existing <- fetchCached subject
    let applyFields record = record
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

-- Cache-first lookup: fresh rows are served directly; missing/stale rows
-- trigger a Confluence search whose outcome (including negatives) is cached.
lookupForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
lookupForAlert = resolve False

-- Manual refresh: bypasses TTL, still serves the stale row on failure.
refreshForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
refreshForAlert = resolve True

resolve :: (?modelContext :: ModelContext) => Bool -> Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
resolve force source alert = case subjectOf alert of
    Nothing -> pure (Right Nothing)
    Just subject -> do
        configResult <- cmdbConfigFromEnv source
        case configResult of
            Nothing -> pure (Left "confluence not configured")
            Just config -> do
                now <- getCurrentTime
                cached <- fetchCached subject
                let ttl = case cached of
                        Just entry | isNothing entry.pageId -> negativeTtlSeconds
                        _ -> positiveTtlSeconds
                    fresh = case cached of
                        Just entry -> isFresh now entry.fetchedAt ttl
                        Nothing -> False
                if fresh && not force
                    then pure (Right cached)
                    else do
                        result <- confluenceSearch config (cqlForSubject config.space (subjectTerm subject))
                        case result of
                            Left err -> pure (Left err) -- stale row still rendered by the card
                            Right pages -> do
                                entry <- upsertEntry subject (pickBestPage (subjectTerm subject) pages) config
                                pure (Right (Just entry))
