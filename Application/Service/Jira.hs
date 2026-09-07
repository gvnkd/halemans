module Application.Service.Jira
( JiraIssue (..)
, JiraConfig (..)
, jiraConfigFromEnv
, jiraEnvConfig
, apiUrl
, jqlForAlert
, searchIssues
, getIssue
, createIssue
, autoLinkForAlert
, createTicketForAlert
, syncOpenLinks
, connectionOk
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import Data.Aeson (Value, object, (.=), (.:), (.:?), (.!=))
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

-- Jira REST v3 client (design_docs/milestone_3.md §5). Auto-link on alert
-- creation, 5-min status sync, manual ticket creation; never auto-creates.

data JiraConfig = JiraConfig
    { baseUrl :: Text
    , token :: Text
    , project :: Text
    , apiVersion :: Text
    } deriving (Eq, Show)

jiraConfigFromEnv :: Source -> IO (Maybe JiraConfig)
jiraConfigFromEnv source =
    jiraEnvConfig (configText "jiraProject" source.config |> fromMaybe "DEV")

jiraEnvConfig :: Text -> IO (Maybe JiraConfig)
jiraEnvConfig project = do
    url <- lookupEnv "HALEMANS_JIRA_URL"
    token <- lookupEnv "JIRA_TOKEN"
    version <- lookupEnv "HALEMANS_JIRA_API_VERSION"
    pure case (url, token) of
        (Just url, Just token) -> Just JiraConfig { baseUrl = cs url, token = cs token, project, apiVersion = maybe defaultApiVersion cs version }
        _ -> Nothing

-- Jira Cloud serves REST v3; Server/Data Center only has v2
-- (HALEMANS_JIRA_API_VERSION=2 there).
defaultApiVersion :: Text
defaultApiVersion = "3"

apiUrl :: JiraConfig -> Text -> Text
apiUrl config path =
    Text.dropWhileEnd (== '/') config.baseUrl <> "/rest/api/" <> config.apiVersion <> path

configText :: Text -> Value -> Maybe Text
configText key value = parseMaybe (Aeson.withObject "config" (\o -> o .: Key.fromText key)) value

data JiraIssue = JiraIssue
    { issueKey :: Text
    , issueSummary :: Text
    , issueStatus :: Text
    , issueStatusCategory :: Text
    , issueLabels :: [Text]
    } deriving (Eq, Show)

instance Aeson.FromJSON JiraIssue where
    parseJSON = Aeson.withObject "JiraIssue" \o -> do
        issueKey <- o .: "key"
        fields <- o .: "fields"
        issueSummary <- fields .:? "summary" .!= ""
        issueLabels <- fields .:? "labels" .!= []
        status <- fields .:? "status"
        (issueStatus, issueStatusCategory) <- case status of
            Just s -> do
                name <- s .:? "name" .!= ""
                category <- s .:? "statusCategory"
                categoryKey <- case category of
                    Just c -> c .:? "key" .!= ""
                    Nothing -> pure ""
                pure (name, categoryKey)
            Nothing -> pure ("", "")
        pure JiraIssue { .. }

jqlForAlert :: Text -> Alert -> Text
jqlForAlert project alert =
    "project = " <> project <> " AND statusCategory != Done AND (" <> Text.intercalate " OR " terms <> ")"
    where
        subjectTerms = mapMaybe (\term -> term)
            [ alert.host <&> (\host -> "labels ~ " <> host)
            , alert.checkName <&> (\check -> "text ~ \"" <> check <> "\"")
            ]
        terms = if null subjectTerms then ["text ~ \"" <> alert.title <> "\""] else subjectTerms

authOpts :: JiraConfig -> Wreq.Options
authOpts config = Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer " <> cs config.token]

searchIssues :: JiraConfig -> Text -> Int -> IO (Either Text [JiraIssue])
searchIssues config jql maxResults = do
    let opts = authOpts config
            & Wreq.param "jql" .~ [jql]
            & Wreq.param "maxResults" .~ [tshow maxResults]
    result <- try (Http.getFollowing opts (cs (apiUrl config "/search")))
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case Aeson.eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded -> do
                let issues = parseMaybe (Aeson.withObject "search" (.: "issues")) decoded
                pure (maybe (Left "jira search: no issues field") Right issues)

getIssue :: JiraConfig -> Text -> IO (Either Text JiraIssue)
getIssue config key = do
    result <- try (Http.getFollowing (authOpts config) (cs (apiUrl config ("/issue/" <> key))))
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case Aeson.eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right issue -> pure (Right issue)

createIssue :: JiraConfig -> Text -> Text -> Text -> IO (Either Text Text)
createIssue config issueType summary description = do
    let body = object
            [ "fields" .= object
                [ "project" .= object ["key" .= config.project]
                , "summary" .= summary
                , "description" .= description
                , "issuetype" .= object ["name" .= issueType]
                ]
            ]
    result <- try (Http.postFollowing (authOpts config) (cs (apiUrl config "/issue")) body)
    case result of
        Left err -> pure (Left (tshow (err :: SomeException)))
        Right response -> case Aeson.eitherDecode (response ^. Wreq.responseBody) of
            Left err -> pure (Left (cs err))
            Right decoded -> do
                let key = parseMaybe (Aeson.withObject "issue" (.: "key")) decoded
                pure (maybe (Left "jira create: no key in response") Right key)

connectionOk :: JiraConfig -> IO (Either Text ())
connectionOk config = do
    result <- try (Http.getFollowing (authOpts config) (cs (apiUrl config "/myself")))
    pure case result of
        Left err -> Left (tshow (err :: SomeException))
        Right _ -> Right ()

issueUrl :: JiraConfig -> Text -> Text
issueUrl config key = Text.dropWhileEnd (== '/') config.baseUrl <> "/browse/" <> key

upsertLink :: (?modelContext :: ModelContext) => JiraConfig -> Id Alert -> Text -> JiraIssue -> IO JiraLink
upsertLink config alertId origin issue = do
    now <- getCurrentTime
    existing <- query @JiraLink
        |> filterWhere (#alertId, alertId)
        |> filterWhere (#ticketKey, issue.issueKey)
        |> fetchOneOrNothing
    let applyFields record = record
            |> set #summary issue.issueSummary
            |> set #status issue.issueStatus
            |> set #url (issueUrl config issue.issueKey)
            |> set #syncedAt now
    case existing of
        Just link -> updateRecord (applyFields link)
        Nothing -> createRecord (applyFields (newRecord @JiraLink
            |> set #alertId alertId
            |> set #ticketKey issue.issueKey
            |> set #origin origin))

-- Auto-link (milestone_3.md §5): top 5 open tickets matching the alert
-- subject become jira_links rows with origin 'auto'.
autoLinkForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text [JiraLink])
autoLinkForAlert source alert = do
    configResult <- jiraConfigFromEnv source
    case configResult of
        Nothing -> pure (Left "jira not configured")
        Just config -> do
            result <- searchIssues config (jqlForAlert config.project alert) 5
            case result of
                Left err -> pure (Left err)
                Right issues -> Right <$> mapM (upsertLink config (get #id alert) "auto") issues

createTicketForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> Text -> Text -> Text -> IO (Either Text JiraLink)
createTicketForAlert source alert issueType summary body = do
    configResult <- jiraConfigFromEnv source
    case configResult of
        Nothing -> pure (Left "jira not configured")
        Just config -> do
            result <- createIssue config issueType summary body
            case result of
                Left err -> pure (Left err)
                Right key -> do
                    issueResult <- getIssue config key
                    case issueResult of
                        Right issue -> Right <$> upsertLink config (get #id alert) "manual" issue
                        Left _ -> do
                            now <- getCurrentTime
                            link <- newRecord @JiraLink
                                |> set #alertId (get #id alert)
                                |> set #ticketKey key
                                |> set #summary summary
                                |> set #status ""
                                |> set #url (issueUrl config key)
                                |> set #origin "manual"
                                |> set #syncedAt now
                                |> createRecord
                            pure (Right link)

-- JiraSyncJob body (§5): refresh status/summary of every link whose alert is
-- not closed. Returns the number of links refreshed.
syncOpenLinks :: (?modelContext :: ModelContext) => IO Int
syncOpenLinks = do
    openAlerts <- query @Alert
        |> filterWhereNot (#status, "closed" :: Text)
        |> fetch
    links <- case openAlerts of
        [] -> pure []
        alerts -> query @JiraLink
            |> filterWhereIn (#alertId, map (get #id) alerts)
            |> fetch
    sources <- query @Source |> fetch
    configs <- forM sources jiraConfigFromEnv
    let config = foldr (<|>) Nothing configs
    case config of
        Nothing -> pure 0
        Just cfg -> do
            refreshed <- forM links \link -> do
                result <- getIssue cfg link.ticketKey
                case result of
                    Left _ -> pure False
                    Right issue -> do
                        now <- getCurrentTime
                        _ <- link
                            |> set #summary issue.issueSummary
                            |> set #status issue.issueStatus
                            |> set #syncedAt now
                            |> updateRecord
                        pure True
            pure (length (filter (\did -> did) refreshed))
