module Application.Service.Llm.Prompt (
    PromptInputs (..),
    emptyInputs,
    renderTemplate,
    bindingsFor,
    templateSlotNames,
    collectAssetAttrs,
    charBudgetForTokens,
    fitPrompt,
    truncationMarker,
    sha256Hex,
    BuiltPrompt (..),
    buildPromptForAlert,
) where

import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Service.Assets.Attrs (configuredAttrNames, objectAttributes)
import qualified Application.Service.Assets.Cache as AssetsCache
import Application.Service.Llm.Output (outputContract)
import qualified Data.Aeson as Aeson
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.List (nubBy)
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch, fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import "cryptonite" Crypto.Hash (SHA256 (..), hashWith)

-- Prompt construction (design_docs/milestone_4.md §5): the active
-- llm_prompt_templates row for name "alert_enrichment" is filled with alert
-- fields, recent events, CMDB excerpt, similar past alerts and Jira links.
--
-- Token budget: hard cap in prompt tokens, counted with the chars/4 heuristic
-- (no tokenizer dependency — milestone_4.md §12). Truncation order when over
-- budget (milestone_8.md §6): similar_alerts -> events -> assets (excerpt,
-- then the granular slots) -> cmdb_excerpt -> jira_links -> description. Title, severity, labels and
-- annotations are never truncated.
-- Truncation points are marked with truncationMarker in the rendered prompt.

truncationMarker :: Text
truncationMarker = "…[truncated]"

data PromptInputs = PromptInputs
    { piTitle :: Text
    , piSeverity :: Text
    , piEnv :: Text
    , piHost :: Text
    , piService :: Text
    , piCheckName :: Text
    , piDescription :: Text
    , piLabels :: Text
    , piAnnotations :: Text
    , piEvents :: Text
    , piCmdbExcerpt :: Text
    , piAssetsExcerpt :: Text
    , piAssetsCount :: Text
    , piAssetsLabels :: Text
    , piAssetsTypes :: Text
    , piAssetsAttrs :: [(Text, Text)]
    , piSimilarAlerts :: Text
    , piJiraLinks :: Text
    , piLanguage :: Text
    }
    deriving (Eq, Show)

emptyInputs :: PromptInputs
emptyInputs = PromptInputs "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" [] "" "" ""

renderTemplate :: Text -> [(Text, Text)] -> Text
renderTemplate body bindings = foldl' step body bindings
  where
    step acc (key, value) = Text.replace ("{{" <> key <> "}}") value acc

bindingsFor :: PromptInputs -> [(Text, Text)]
bindingsFor inputs =
    [ ("alert.title", inputs.piTitle)
    , ("alert.severity", inputs.piSeverity)
    , ("alert.env", inputs.piEnv)
    , ("alert.host", inputs.piHost)
    , ("alert.service", inputs.piService)
    , ("alert.check_name", inputs.piCheckName)
    , ("alert.description", inputs.piDescription)
    , ("alert.labels", inputs.piLabels)
    , ("alert.annotations", inputs.piAnnotations)
    , ("events", inputs.piEvents)
    , ("cmdb_excerpt", inputs.piCmdbExcerpt)
    , ("assets_excerpt", inputs.piAssetsExcerpt)
    , ("assets.count", inputs.piAssetsCount)
    , ("assets.labels", inputs.piAssetsLabels)
    , ("assets.types", inputs.piAssetsTypes)
    , ("similar_alerts", inputs.piSimilarAlerts)
    , ("jira_links", inputs.piJiraLinks)
    , ("language", inputs.piLanguage)
    ]
        ++ map (\(name, value) -> ("assets.attr." <> name, value)) inputs.piAssetsAttrs

-- Slot names for the admin template editor help text. Static slots come from
-- bindingsFor so the docs cannot drift from the renderer; the dynamic
-- per-attribute family is appended as a pattern.
templateSlotNames :: [Text]
templateSlotNames = map fst (bindingsFor emptyInputs) ++ ["assets.attr.<AttributeName>"]

charBudgetForTokens :: Int -> Int
charBudgetForTokens tokens = tokens * 4

-- Fill + budget enforcement. Truncates fields in the documented order until
-- the rendered prompt fits the char budget; the final hard fallback trims the
-- rendered text itself so the cap can never be exceeded.
fitPrompt :: Int -> Text -> PromptInputs -> Text
fitPrompt tokenBudget template inputs = go inputs truncatable
  where
    budget = charBudgetForTokens tokenBudget
    renderedWith i = renderTemplate template (bindingsFor i)
    go current []
        | Text.length (renderedWith current) <= budget = renderedWith current
        | otherwise = Text.take budget (renderedWith current)
    go current (field : rest)
        | Text.length (renderedWith current) <= budget = renderedWith current
        | otherwise = go (field (Text.length (renderedWith current) - budget) current) rest
    truncatable =
        [ \excess i -> i{piSimilarAlerts = shrink excess i.piSimilarAlerts}
        , \excess i -> i{piEvents = shrink excess i.piEvents}
        , \excess i -> i{piAssetsExcerpt = shrink excess i.piAssetsExcerpt}
        , \excess i -> i{piAssetsLabels = shrink excess i.piAssetsLabels}
        , \excess i -> i{piAssetsTypes = shrink excess i.piAssetsTypes}
        , \excess i -> i{piAssetsAttrs = map (\(name, value) -> (name, shrink excess value)) i.piAssetsAttrs}
        , \excess i -> i{piCmdbExcerpt = shrink excess i.piCmdbExcerpt}
        , \excess i -> i{piJiraLinks = shrink excess i.piJiraLinks}
        , \excess i -> i{piDescription = shrink excess i.piDescription}
        ]
    shrink excess value
        | excess <= 0 = value
        | Text.length value <= excess + Text.length truncationMarker = ""
        | otherwise = Text.take (Text.length value - excess) value <> truncationMarker

sha256Hex :: Text -> Text
sha256Hex input = cs (convertToBase Base16 digest :: ByteString)
  where
    digest = hashWith SHA256 (cs input :: ByteString)

data BuiltPrompt = BuiltPrompt
    { templateId :: Id LlmPromptTemplate
    , templateVersion :: Int
    , rendered :: Text
    , hash :: Text
    }
    deriving (Eq, Show)

-- Reads whatever context is present at build time (milestone_4.md §4: no
-- ordering dependency on EnrichAlertJob; absent context renders as empty
-- sections). The template name comes from the resolved agent role
-- (milestone_8.md §7; "alert_enrichment" when no role applies). The language
-- name fills the {{language}} slot (profile language of the queueing user, or
-- the HALEMANS_DEFAULT_LANGUAGE fallback — resolved by the caller).
buildPromptForAlert :: (?modelContext :: ModelContext) => Text -> Int -> Text -> Alert -> IO (Maybe BuiltPrompt)
buildPromptForAlert languageName tokenBudget templateName alert = do
    template <-
        query @LlmPromptTemplate
            |> filterWhere (#name, templateName)
            |> filterWhere (#active, True)
            |> fetchOneOrNothing
    forM template \template -> do
        inputs <- gatherInputs alert
        let rendered =
                fitPrompt tokenBudget template.body inputs{piLanguage = languageName}
                    <> outputContract
        pure
            BuiltPrompt
                { templateId = get #id template
                , templateVersion = template.version
                , rendered
                , hash = sha256Hex rendered
                }

gatherInputs :: (?modelContext :: ModelContext) => Alert -> IO PromptInputs
gatherInputs alert = do
    events <-
        query @AlertEvent
            |> filterWhere (#alertId, get #id alert)
            |> orderByDesc #createdAt
            |> limit 10
            |> fetch
    cmdbEntry <- case (alert.hostId, alert.serviceId) of
        (Just hostId, _) ->
            query @CmdbEntry
                |> filterWhere (#hostId, Just hostId)
                |> fetchOneOrNothing
        (Nothing, Just serviceId) ->
            query @CmdbEntry
                |> filterWhere (#serviceId, Just serviceId)
                |> fetchOneOrNothing
        (Nothing, Nothing) -> pure Nothing
    let baseSimilar =
            query @Alert
                |> filterWhereNot (#id, get #id alert)
                |> filterWhereIn (#status, ["resolved", "closed"] :: [Text])
                |> orderByDesc #resolvedAt
                |> limit 5
    byFingerprint <-
        baseSimilar
            |> filterWhere (#fingerprint, alert.fingerprint)
            |> fetch
    byCheckName <- case alert.checkName of
        Just checkName ->
            baseSimilar
                |> filterWhere (#checkName, Just checkName)
                |> fetch
        Nothing -> pure []
    let similar = take 5 (nubBy (\a b -> get #id a == get #id b) (byFingerprint ++ byCheckName))
    jiraLinks <-
        query @JiraLink
            |> filterWhere (#alertId, get #id alert)
            |> orderByDesc #createdAt
            |> limit 5
            |> fetch
    linkedAssets <- AssetsCache.linkedAssetsForAlert alert
    assetConfigs <- forM linkedAssets \(_, object) -> fetch object.configId
    pure
        emptyInputs
            { piTitle = alert.title
            , piSeverity = alert.severity
            , piEnv = fromMaybe "unknown" (effectiveFieldText FieldEnv alert)
            , piHost = fromMaybe "unknown" (effectiveFieldText FieldHost alert)
            , piService = fromMaybe "unknown" (effectiveFieldText FieldService alert)
            , piCheckName = fromMaybe "unknown" alert.checkName
            , piDescription = alert.description
            , piLabels = cs (Aeson.encode alert.labels)
            , piAnnotations = cs (Aeson.encode alert.annotations)
            , piEvents = Text.intercalate "\n" (map eventLine events)
            , piCmdbExcerpt = maybe "" (.excerpt) cmdbEntry
            , piAssetsExcerpt = Text.intercalate "\n" (zipWith assetLine linkedAssets assetConfigs)
            , piAssetsCount = tshow (length linkedAssets)
            , piAssetsLabels = Text.intercalate ", " (map (\(_, object) -> object.label_) linkedAssets)
            , piAssetsTypes = Text.intercalate ", " (nub (map (\(_, object) -> object.objectTypeName) linkedAssets))
            , piAssetsAttrs = collectAssetAttrs (map snd linkedAssets)
            , piSimilarAlerts = Text.intercalate "\n" (map similarLine similar)
            , piJiraLinks = Text.intercalate "\n" (map jiraLine jiraLinks)
            }

-- Assets excerpt line (milestone_8.md §6): label, object type, status and the
-- config's display attributes (owner/cluster/IPs/datacenter by default).
assetLine :: (AssetAlertLink, AssetsObject) -> AssetsConfig -> Text
assetLine (_, object) config =
    mconcat
        [ "- "
        , object.label_
        , " ["
        , object.objectTypeName
        , "]"
        , maybe "" (" status=" <>) (lookupAttr "Status")
        , Text.concat (map field (configuredAttrNames config))
        ]
  where
    attrs = objectAttributes object
    lookupAttr name = lookup name attrs
    field name = case lookupAttr name of
        Just value | not (Text.null value) -> " | " <> name <> ": " <> value
        _ -> ""

-- Granular asset slots ({{assets.count}}/{{assets.labels}}/{{assets.types}}/
-- {{assets.attr.<Name>}}): attribute values aggregated across linked objects,
-- first-seen name order, distinct non-empty values comma-joined. Names
-- containing braces are skipped so they can never break the {{...}} syntax.
collectAssetAttrs :: [AssetsObject] -> [(Text, Text)]
collectAssetAttrs objects = map joinValues attrNames
  where
    pairs = concatMap objectAttributes objects
    attrNames = filter (Text.all (\c -> c /= '{' && c /= '}')) (nub (map fst pairs))
    joinValues name =
        let values = nub [value | (key, value) <- pairs, key == name, not (Text.null value)]
         in (name, Text.intercalate ", " values)

eventLine :: AlertEvent -> Text
eventLine event = "- " <> tshow event.createdAt <> " " <> event.kind

similarLine :: Alert -> Text
similarLine past =
    mconcat
        [ "- "
        , past.title
        , " ["
        , past.severity
        , "]"
        , maybe "" (\at -> " resolved " <> tshow at) past.resolvedAt
        , maybe "" (\reason -> "; close reason: " <> reason) past.closeReason
        , maybe "" (\comment -> "; ack: " <> comment) past.ackComment
        ]

jiraLine :: JiraLink -> Text
jiraLine link = "- " <> link.ticketKey <> " " <> link.summary <> " [" <> link.status <> "]"
