module Application.Service.Llm.Tools
( toolDefinitions
, executeToolCall
) where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import Data.Aeson (Value, object, (.=), (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Jira as Jira
import Application.Service.Llm (ToolCall (..))

-- Optional read-only tool access for the model (design_docs/milestone_4.md
-- D4a), exposed via OpenAI-style tool calling. Tools only ever READ from the
-- same M3 service clients that back the card panels; nothing here mutates
-- alert state or calls external write APIs (milestone_4.md D8).

toolDefinitions :: [Value]
toolDefinitions =
    [ object
        [ "type" .= ("function" :: Text)
        , "function" .= object
            [ "name" .= ("cmdb_lookup" :: Text)
            , "description" .= ("Look up configuration items in the CMDB (Confluence) by search term" :: Text)
            , "parameters" .= object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "term" .= object ["type" .= ("string" :: Text), "description" .= ("host or service name to search for" :: Text)]
                    ]
                , "required" .= (["term"] :: [Text])
                ]
            ]
        ]
    , object
        [ "type" .= ("function" :: Text)
        , "function" .= object
            [ "name" .= ("jira_search" :: Text)
            , "description" .= ("Search Jira tickets by text query" :: Text)
            , "parameters" .= object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "query" .= object ["type" .= ("string" :: Text), "description" .= ("free-text query" :: Text)]
                    ]
                , "required" .= (["query"] :: [Text])
                ]
            ]
        ]
    ]

-- Executes one model-requested tool call; result is returned as the text
-- content of a role=tool message. Failures are reported in-band as text so a
-- broken CMDB/Jira never fails the analysis (soft-fail, D8).
executeToolCall :: (?modelContext :: ModelContext) => Maybe Source -> ToolCall -> IO Text
executeToolCall source call = case call.callName of
    "cmdb_lookup" -> withTextArg "term" (cmdbLookup source)
    "jira_search" -> withTextArg "query" (jiraSearch source)
    other -> pure ("unknown tool: " <> other)
    where
        withTextArg key run = case textArg key call.callArguments of
            Nothing -> pure ("invalid arguments for " <> call.callName)
            Just value -> run value

textArg :: Text -> Text -> Maybe Text
textArg key raw = do
    decoded <- Aeson.decode (cs raw) :: Maybe Value
    parseMaybe (Aeson.withObject "arguments" (\o -> o .: Key.fromText key)) decoded

cmdbLookup :: (?modelContext :: ModelContext) => Maybe Source -> Text -> IO Text
cmdbLookup Nothing _ = pure "cmdb unavailable: no source"
cmdbLookup (Just source) term = do
    configResult <- Cmdb.cmdbConfigFromEnv source
    case configResult of
        Nothing -> pure "cmdb not configured"
        Just config -> do
            result <- Cmdb.confluenceSearch config (Cmdb.cqlForSubject config.space term)
            pure case result of
                Left err -> "cmdb lookup failed: " <> err
                Right pages -> renderPages pages
    where
        renderPages [] = "no cmdb pages found"
        renderPages pages = Text.intercalate "\n" (map renderPage (take 3 pages))
        renderPage page = "- " <> page.pageTitle <> ": "
            <> Text.take 200 (Cmdb.excerptFromHtml 200 page.pageBodyHtml)

jiraSearch :: (?modelContext :: ModelContext) => Maybe Source -> Text -> IO Text
jiraSearch Nothing _ = pure "jira unavailable: no source"
jiraSearch (Just source) queryText = do
    configResult <- Jira.jiraConfigFromEnv source
    case configResult of
        Nothing -> pure "jira not configured"
        Just config -> do
            let jql = "project = " <> config.project <> " AND text ~ \"" <> queryText <> "\""
            result <- Jira.searchIssues config jql 5
            pure case result of
                Left err -> "jira search failed: " <> err
                Right issues -> renderIssues issues
    where
        renderIssues [] = "no jira tickets found"
        renderIssues issues = Text.intercalate "\n"
            (map (\issue -> "- " <> issue.issueKey <> " " <> issue.issueSummary <> " [" <> issue.issueStatus <> "]") issues)
