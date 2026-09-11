module Application.Service.Jira.Related
( relatedTasksForAlert
, relevancePrompt
, parseRelevantKeys
, maxCandidates
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext, newRecord, createRecord, updateRecord, deleteRecord)
import IHP.HaskellSupport (set, get)
import IHP.QueryBuilder (query, filterWhere)
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import qualified Application.Service.Jira as Jira
import Application.Service.Jira (JiraIssue (..))
import qualified Application.Service.Jira.DbConfig as JiraDb
import qualified Application.Service.Assets as Assets
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.Assets.Types (Ticket (..))
import Application.Service.Llm (OpenAiCompat (..), LlmProvider (..), Prompt (..), userMessage, Completion (..))
import Application.Service.Llm.DbConfig (currentLlmConfig)
import Application.Service.Llm.Output (parseCompletionOutput, ParsedOutput (..))
import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Functor ((<&>))

-- Related Jira tasks (milestone 10): candidates are gathered from Jira issue
-- search across ALL configured projects AND from Jira Assets "linked tasks"
-- (objectconnectedtickets) of the alert's linked assets. The configured LLM
-- then keeps only tasks relevant to the alert; the survivors are stored as
-- jira_links rows with origin 'related' and shown in the Jira card's
-- "Related tasks" section. Re-runs converge: related rows the LLM no longer
-- selects are deleted.

maxCandidates :: Int
maxCandidates = 15

data RelatedCandidate = RelatedCandidate
    { candidateKey :: Text
    , candidateSummary :: Text
    , candidateStatus :: Text
    , candidateUrl :: Text
    }

relatedTasksForAlert :: (?modelContext :: ModelContext) => Maybe Source -> Alert -> IO (Either Text ())
relatedTasksForAlert maybeSource alert = do
    configs <- case maybeSource of
        Just source -> JiraDb.jiraConfigsForSource source
        Nothing -> JiraDb.currentJiraConfigs
    jiraResults <- forM configs \config -> do
        -- Unlike auto-link there is no statusCategory filter: related tasks
        -- deliberately include historical (Done) tickets about the same
        -- subject.
        result <- Jira.searchIssues config (Jira.projectClause (Jira.projects config) <> "(" <> Jira.jqlSubjectTerms alert <> ")") 10
        pure (result <&> map (fromIssue config))
    let jiraCandidates = concat [issues | Right issues <- jiraResults]
    assetsCandidates <- assetsTicketCandidates configs alert
    let candidates = dedupeOn candidateKey (jiraCandidates ++ assetsCandidates)
    existing <- query @JiraLink
        |> filterWhere (#alertId, get #id alert)
        |> fetch
    let linkedKeys = [link.ticketKey | link <- existing, link.origin /= "related"]
        fresh = filter (\candidate -> candidate.candidateKey `notElem` linkedKeys) (take maxCandidates candidates)
    llmVerdict <- filterWithLlm alert fresh
    let kept = case llmVerdict of
            Just keys -> filter (\candidate -> candidate.candidateKey `elem` keys) fresh
            Nothing -> fresh -- no LLM configured or call failed: keep candidates unfiltered
    now <- getCurrentTime
    forM_ kept \candidate -> do
        existingLink <- query @JiraLink
            |> filterWhere (#alertId, get #id alert)
            |> filterWhere (#ticketKey, candidate.candidateKey)
            |> fetchOneOrNothing
        let applyFields record = record
                |> set #summary candidate.candidateSummary
                |> set #status candidate.candidateStatus
                |> set #url candidate.candidateUrl
                |> set #syncedAt now
        _ <- case existingLink of
            Just link -> updateRecord (applyFields link)
            Nothing -> createRecord (applyFields (newRecord @JiraLink
                |> set #alertId (get #id alert)
                |> set #ticketKey candidate.candidateKey
                |> set #origin "related"))
        pure ()
    let keepKeys = map (.candidateKey) kept
        stale = [link | link <- existing, link.origin == "related", link.ticketKey `notElem` keepKeys]
    forM_ stale deleteRecord
    -- Total search failure (configs exist but every one errored) is reported
    -- so the enrichment job can log an enrichment_failed event; partial
    -- failures and the LLM filter stay silent (soft-fail, advisory data).
    pure case [err | Left err <- jiraResults] of
        errs | not (null errs) && length errs == length jiraResults -> Left (fromMaybe "jira search failed" (head errs))
        _ -> Right ()

fromIssue :: Jira.JiraConfig -> JiraIssue -> RelatedCandidate
fromIssue config issue = RelatedCandidate
    { candidateKey = issue.issueKey
    , candidateSummary = issue.issueSummary
    , candidateStatus = issue.issueStatus
    , candidateUrl = Jira.issueUrl config issue.issueKey
    }

-- Assets "linked tasks" (milestone 10): tickets connected to each linked
-- assets object. The browse URL is derived from the first Jira config when
-- one exists, else from the assets connection's Jira origin.
assetsTicketCandidates :: (?modelContext :: ModelContext) => [Jira.JiraConfig] -> Alert -> IO [RelatedCandidate]
assetsTicketCandidates jiraConfigs alert = do
    linked <- AssetsCache.linkedAssetsForAlert alert
    concat <$> forM linked \(_, object) -> do
        config <- fetchOneOrNothing object.configId
        case config of
            Nothing -> pure []
            Just assetsConfig -> do
                clientResult <- Assets.clientFromConfig assetsConfig
                case clientResult of
                    Left _ -> pure []
                    Right client -> do
                        result <- Assets.connectedTickets client (fromIntegral object.objectId)
                        pure case result of
                            Left _ -> []
                            Right tickets -> map (fromTicket (browseBase jiraConfigs assetsConfig)) tickets
    where
        browseBase jiraConfigs assetsConfig = case jiraConfigs of
            (config:_) -> Text.dropWhileEnd (== '/') (Jira.baseUrl config)
            [] -> Text.dropWhileEnd (== '/')
                (fromMaybe assetsConfig.baseUrl (Text.stripSuffix "/rest/assets/latest" assetsConfig.baseUrl))
        fromTicket base ticket = RelatedCandidate
            { candidateKey = ticket.ticketKey
            , candidateSummary = ticket.ticketSummary
            , candidateStatus = ticket.ticketStatus
            , candidateUrl = base <> "/browse/" <> ticket.ticketKey
            }

dedupeOn :: Eq b => (a -> b) -> [a] -> [a]
dedupeOn key = go []
    where
        go _ [] = []
        go seen (x:xs)
            | key x `elem` seen = go seen xs
            | otherwise = x : go (key x : seen) xs

-- LLM relevance filter: returns Just keys-to-keep when the configured LLM
-- answered with a parseable verdict (an empty list is a valid "none
-- relevant" verdict), Nothing when no LLM is configured or the call/parse
-- failed (caller keeps candidates unfiltered — soft-fail).
filterWithLlm :: (?modelContext :: ModelContext) => Alert -> [RelatedCandidate] -> IO (Maybe [Text])
filterWithLlm _ [] = pure (Just [])
filterWithLlm alert candidates = do
    maybeConfig <- currentLlmConfig
    case maybeConfig of
        Nothing -> pure Nothing
        Just config -> do
            result <- complete (OpenAiCompat config) (Prompt [userMessage (relevancePrompt alert candidates)] [])
            pure case result of
                Left _ -> Nothing
                Right completion -> parseRelevantKeys (map (.candidateKey) candidates) completion.content

relevancePrompt :: Alert -> [RelatedCandidate] -> Text
relevancePrompt alert candidates = Text.intercalate "\n"
    [ "You are triaging Jira tasks related to a monitoring alert."
    , ""
    , "Alert: " <> alert.title
    , "Host: " <> fromMaybe "-" alert.host <> "; Service: " <> fromMaybe "-" alert.service <> "; Check: " <> fromMaybe "-" alert.checkName
    , "Severity: " <> alert.severity
    , "Description: " <> Text.take 500 alert.description
    , ""
    , "## Candidate Jira tasks"
    , Text.intercalate "\n" (map candidateLine candidates)
    , ""
    , "Keep only tasks plausibly related to this alert (same host, service or failure mode)."
    , "Respond with exactly one ```json fenced block of the shape {\"relevant\": [\"KEY-1\", ...]}."
    , "Use only keys from the candidate list; an empty list means none are relevant."
    ]
    where
        candidateLine candidate = "- " <> candidate.candidateKey <> ": " <> candidate.candidateSummary <> " [" <> candidate.candidateStatus <> "]"

-- Extracts the {"relevant": [...]} verdict from the fenced json block,
-- intersected with the actual candidate keys so hallucinated keys drop out.
parseRelevantKeys :: [Text] -> Text -> Maybe [Text]
parseRelevantKeys candidateKeys raw = do
    structured <- (parseCompletionOutput raw).structured
    keys <- parseMaybe (Aeson.withObject "verdict" (.: "relevant")) structured
    pure (filter (`elem` candidateKeys) keys)
