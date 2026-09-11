module Application.Service.Llm.Tools
( toolDefinitions
, executeToolCall
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)
import Generated.Types
import Data.Aeson (Value, object, (.=), (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import qualified Application.Service.Cmdb as Cmdb
import qualified Application.Service.Cmdb.DbConfig as CmdbDb
import qualified Application.Service.Jira as Jira
import qualified Application.Service.Jira.DbConfig as JiraDb
import Application.Service.Jira (JiraIssue (..))
import Application.Service.Cmdb (ConfPage (..))
import qualified Application.Service.Assets as Assets
import qualified Application.Service.Assets.Aql as Aql
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.Assets.Types (ObjectListResult (..), AssetObject (..), ObjectAttribute (..), ObjectAttributeValue (..))
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
    , object
        [ "type" .= ("function" :: Text)
        , "function" .= object
            [ "name" .= ("assets_lookup" :: Text)
            , "description" .= ("Look up assets (hosts, databases, clusters) in Jira Assets by search term" :: Text)
            , "parameters" .= object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "term" .= object ["type" .= ("string" :: Text), "description" .= ("host or asset name to search for" :: Text)]
                    ]
                , "required" .= (["term"] :: [Text])
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
    "assets_lookup" -> withTextArg "term" assetsLookup
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
    configs <- CmdbDb.cmdbConfigsForSource source
    if null configs
        then pure "cmdb not configured"
        else do
            results <- forM configs \config ->
                Cmdb.confluenceSearch config (Cmdb.cqlForSubject (Cmdb.spaces config) term)
            pure case concat [pages | Right pages <- results] of
                [] | all isLeft results -> "cmdb lookup failed: " <> renderFirstErr results
                [] -> "no cmdb pages found"
                pages -> renderPages pages
    where
        isLeft (Left _) = True
        isLeft _ = False
        renderFirstErr results = fromMaybe "unknown error" (head [err | Left err <- results])
        renderPages pages = Text.intercalate "\n" (map renderPage (take 3 pages))
        renderPage page = "- " <> page.pageTitle <> ": "
            <> Text.take 200 (Cmdb.excerptFromHtml 200 page.pageBodyHtml)

jiraSearch :: (?modelContext :: ModelContext) => Maybe Source -> Text -> IO Text
jiraSearch Nothing _ = pure "jira unavailable: no source"
jiraSearch (Just source) queryText = do
    configs <- JiraDb.jiraConfigsForSource source
    if null configs
        then pure "jira not configured"
        else do
            results <- forM configs \config ->
                Jira.searchIssues config (Jira.projectClause (Jira.projects config) <> "text ~ \"" <> queryText <> "\"") 5
            pure case concat [issues | Right issues <- results] of
                [] | all isLeft results -> "jira search failed: " <> renderFirstErr results
                [] -> "no jira tickets found"
                issues -> renderIssues (take 5 issues)
    where
        isLeft (Left _) = True
        isLeft _ = False
        renderFirstErr results = fromMaybe "unknown error" (head [err | Left err <- results])
        renderIssues issues = Text.intercalate "\n"
            (map (\issue -> "- " <> issue.issueKey <> " " <> issue.issueSummary <> " [" <> issue.issueStatus <> "]") issues)

-- assets_lookup (milestone_8.md §6): AQL-backed read-only search against the
-- default (first enabled) assets_configs row; failures come back in-band as
-- text, exactly like cmdb_lookup/jira_search.
assetsLookup :: (?modelContext :: ModelContext) => Text -> IO Text
assetsLookup term = do
    maybeConfig <- query @AssetsConfig
        |> filterWhere (#enabled, True)
        |> orderByAsc #name
        |> limit 1
        |> fetchOneOrNothing
    case maybeConfig of
        Nothing -> pure "assets not configured"
        Just config -> do
            clientResult <- Assets.clientFromConfig config
            case clientResult of
                Left err -> pure ("assets lookup failed: " <> err)
                Right client -> do
                    let aql = Aql.fillHostTemplate (AssetsCache.queryTemplate config) term
                    result <- Assets.searchObjects client aql 1 5
                    pure case result of
                        Left err -> "assets lookup failed: " <> Assets.describeError err
                        Right page -> renderObjects page.listEntries
    where
        renderObjects [] = "no assets found"
        renderObjects objects = Text.take 1500 (Text.intercalate "\n" (map renderObject objects))
        renderObject object = mconcat
            [ "- ", object.objectLabel, " (", object.objectKey, ") [", object.objectTypeName, "]"
            , Text.concat (map attrSummary (take 6 object.objectAttributes))
            ]
        attrSummary attribute = case attribute.attrValues of
            [] -> ""
            values -> " | " <> attribute.attrName <> ": "
                <> Text.intercalate ", " (map (.valueDisplay) values)
