module Application.Service.Mattermost.Render (
    MattermostRenderContext (..),
    renderRootMessage,
    renderRootProps,
    renderDetailsMessage,
    renderStatusMessage,
    renderFields,
    renderFieldPairs,
    renderColor,
    MattermostCardPreview (..),
    previewCard,
    sampleRenderContext,
    sampleAlert,
    resolveColor,
    defaultColorMap,
    colorMapFromJson,
    syncsKind,
    defaultRootTemplateBody,
    defaultDetailsTemplateBody,
    defaultStatusTemplateBody,
    defaultFieldsTemplateBody,
    defaultColorTemplateBody,
    mattermostRootTemplateName,
    mattermostDetailsTemplateName,
    mattermostStatusTemplateName,
    mattermostFieldsTemplateName,
    mattermostColorTemplateName,
    mattermostTemplateNames,
    mattermostSlotNames,
) where

import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Service.Llm.Prompt (renderTemplate)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Generated.Types
import IHP.HaskellSupport ((|>))
import IHP.ModelSupport (newRecord)
import IHP.Prelude

-- Pure Mattermost message rendering. The root post carries the templated
-- alert summary as a message attachment (color = severity/state, fields,
-- optional Ack action); the alert details ride in a thread reply so the
-- channel listing stays scannable.

data MattermostRenderContext = MattermostRenderContext
    { mrcRuleName :: Text
    , mrcAckedBy :: Maybe Text
    , mrcAckedAt :: Maybe UTCTime
    -- ^ Ack timestamp for the root status line ("Acked by X at Y").
    , mrcClosedBy :: Maybe Text
    , mrcActionUrl :: Maybe Text
    -- ^ Nothing = Ack button omitted (no public base URL configured).
    , mrcAckUrl :: Maybe Text
    -- ^ One-time markdown Ack link; independent of the interactive button
    -- (works even when the MM server strips action integration URLs).
    , mrcAlertUrl :: Text
    , mrcAlertId :: Text
    }
    deriving (Eq, Show)

-- | AlertEvent kinds worth syncing to the root post. "created" is excluded
-- on purpose: the initial root post is the notify job's job; "comment",
-- "writeback", ... never change the visual state.
syncsKind :: Text -> Bool
syncsKind kind = kind `elem` (["repeated", "resolved", "ack", "unack", "closed", "stalled"] :: [Text])

-- The message texts are DB-templated via llm_prompt_templates rows named
-- mattermost_root / mattermost_details (the prompt-template machinery:
-- versioned rows, admin editor, provision). These constants are the built-in
-- fallback used when no active row exists — a broken or deleted template must
-- never drop the notification. They are slot-templates themselves, so the
-- fallback and the DB path render through one code path.
mattermostRootTemplateName :: Text
mattermostRootTemplateName = "mattermost_root"

mattermostDetailsTemplateName :: Text
mattermostDetailsTemplateName = "mattermost_details"

-- Root-card sub-part templates (llm_prompt_templates rows like the root/details
-- texts): the attachment status line, the fields grid, and the color bar.
-- All fail-soft: a missing/broken row falls back to the built-in default, so
-- a bad edit can garble a card but never drop the notification.
mattermostStatusTemplateName :: Text
mattermostStatusTemplateName = "mattermost_status"

mattermostFieldsTemplateName :: Text
mattermostFieldsTemplateName = "mattermost_fields"

mattermostColorTemplateName :: Text
mattermostColorTemplateName = "mattermost_color"

mattermostTemplateNames :: [Text]
mattermostTemplateNames =
    [ mattermostRootTemplateName
    , mattermostDetailsTemplateName
    , mattermostStatusTemplateName
    , mattermostFieldsTemplateName
    , mattermostColorTemplateName
    ]

defaultRootTemplateBody :: Text
defaultRootTemplateBody = "[{{alert.state}}] {{alert.title}}"

-- The status line is one line per status; exactly one of the line_* slots is
-- non-empty for a given alert, and renderTemplateLines drops the empty lines,
-- so a plain list renders the single applicable line. Admins can restructure
-- freely (e.g. always show the ack link, add the rule name).
defaultStatusTemplateBody :: Text
defaultStatusTemplateBody =
    Text.intercalate
        "\n"
        [ "{{line_ack}}"
        , "{{line_closed}}"
        , "{{line_resolved}}"
        , "{{line_stalled}}"
        , "{{line_firing}}"
        ]

-- Fields grid: one "Title|value" per line; the value side takes the same
-- slots as every other template. Lines whose rendered value is empty are
-- dropped (an absent slot must not leave an empty row). The title side is
-- literal text — it is NOT slot-rendered, so a "|" in a title is impossible
-- by construction.
defaultFieldsTemplateBody :: Text
defaultFieldsTemplateBody =
    Text.intercalate
        "\n"
        [ "Environment|{{alert.env}}"
        , "Host|{{alert.host}}"
        , "Service|{{alert.service}}"
        , "Severity|{{alert.severity}}"
        , "Rule|{{rule}}"
        ]

-- References the severity→color config mapping (notification_channels.config
-- "colors", sane defaults in defaultColorMap) through the {{color}} slot;
-- admins can also hardcode a literal "#RRGGBB" here.
defaultColorTemplateBody :: Text
defaultColorTemplateBody = "{{color}}"

-- Sane defaults for the channel config "colors" mapping: terminal states
-- gray, then severity, then the fallback. resolveColor looks the alert's
-- STATUS up first (so "resolved" wins over "critical"), then its severity,
-- then "default" — mirroring the historic hardcoded behavior.
defaultColorMap :: [(Text, Text)]
defaultColorMap =
    [ ("resolved", "#98A2AD")
    , ("closed", "#98A2AD")
    , ("stalled", "#98A2AD")
    , ("critical", "#E5484D")
    , ("high", "#FF5A1F")
    , ("warning", "#F7B500")
    , ("default", "#4C8DFF")
    ]

resolveColor :: [(Text, Text)] -> Alert -> Text
resolveColor overrides alert =
    fromMaybe "#4C8DFF" (lookup (statusText alert) merged <|> lookup alert.severity merged <|> lookup "default" merged)
  where
    merged = overrides ++ filter ((`notElem` map fst overrides) . fst) defaultColorMap

-- Overrides from a notification_channels.config JSON: an optional "colors"
-- object of string values. Unknown shapes yield no overrides (defaults rule).
colorMapFromJson :: Value -> [(Text, Text)]
colorMapFromJson config = case config of
    Aeson.Object object_ -> case KeyMap.lookup "colors" object_ of
        Just (Aeson.Object colors) ->
            [ (Key.toText key, value)
            | (key, Aeson.String value) <- KeyMap.toList colors
            ]
        _ -> []
    _ -> []

defaultDetailsTemplateBody :: Text
defaultDetailsTemplateBody =
    Text.intercalate
        "\n"
        [ "**{{alert.title}}**"
        , "{{alert.description}}"
        , ""
        , "Severity: {{alert.severity}}"
        , "Status: {{alert.status}}"
        , "Environment: {{alert.env}}"
        , "Host: {{alert.host}}"
        , "Service: {{alert.service}}"
        , "Occurrences: {{alert.occurrences}}"
        , "Fingerprint: `{{alert.fingerprint}}`"
        , ""
        , "[Open in Halemans]({{alert_url}})"
        ]

-- Slot names for the admin template editor help text; kept next to
-- mmBindings so the docs cannot drift from the renderer.
mattermostSlotNames :: [Text]
mattermostSlotNames = map fst (mmBindings [] sampleRenderContext sampleAlert)

-- | Sample render context for the slot-name list and the admin card preview:
-- representative values so every slot renders something meaningful.
sampleRenderContext :: MattermostRenderContext
sampleRenderContext =
    MattermostRenderContext
        { mrcRuleName = "sample-rule"
        , mrcAckedBy = Just "on-call"
        , mrcAckedAt = Nothing
        , mrcClosedBy = Nothing
        , mrcActionUrl = Just "http://halemans.example/hooks/mattermost/actions/secret"
        , mrcAckUrl = Just "http://halemans.example/alerts/sample/ack-link?token=sample"
        , mrcAlertUrl = "http://halemans.example/alerts/sample"
        , mrcAlertId = "sample"
        }

-- | Sample alert for the admin card preview: firing/critical so the preview
-- shows the Ack action and the red bar.
sampleAlert :: Alert
sampleAlert =
    newRecord @Alert
        |> set #title "Sample: CPU load above 80%"
        |> set #fingerprint "sample-fingerprint"
        |> set #status "firing"
        |> set #severity "critical"
        |> set #occurrences 3
        |> set #env (Just "production")
        |> set #host (Just "db-pgsql01")
        |> set #service (Just "postgresql")
        |> set #description "load average 15m above threshold"

-- | The rendered root card, decomposed for the admin preview page. Each
-- Maybe is a template body (Nothing = the built-in default).
data MattermostCardPreview = MattermostCardPreview
    { mcpHeader :: Text
    , mcpStatus :: Text
    , mcpFields :: [(Text, Text)]
    , mcpColor :: Text
    , mcpAckAction :: Bool
    }
    deriving (Eq, Show)

previewCard :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> MattermostCardPreview
previewCard rootTemplate statusTemplate fieldsTemplate colorTemplate =
    MattermostCardPreview
        { mcpHeader = renderRootMessage rootTemplate [] sampleRenderContext sampleAlert
        , mcpStatus = renderStatusMessage statusTemplate [] sampleRenderContext sampleAlert
        , mcpFields = renderFieldPairs fieldsTemplate [] sampleRenderContext sampleAlert
        , mcpColor = renderColor colorTemplate [] sampleRenderContext sampleAlert
        , mcpAckAction = True -- sample context carries an action URL on a firing alert
        }

-- | Bindings shared by all mattermost templates. Alert fields use the
-- alert.* prefix (the LLM template convention); the render-context values
-- are plain names. The color overrides come from the channel config and feed
-- the {{color}} slot (and the color bar's default resolution). line_* are
-- the per-status status-line phrases — exactly one is non-empty, so the
-- default mattermost_status body renders the single applicable line.
mmBindings :: [(Text, Text)] -> MattermostRenderContext -> Alert -> [(Text, Text)]
mmBindings colorOverrides context alert =
    [ ("alert.title", alert.title)
    , ("alert.severity", alert.severity)
    , ("alert.status", statusText alert)
    , ("alert.state", stateLabel (statusText alert))
    , ("alert.env", fieldOrDash FieldEnv alert)
    , ("alert.host", fieldOrDash FieldHost alert)
    , ("alert.service", fieldOrDash FieldService alert)
    , ("alert.occurrences", tshow alert.occurrences)
    , ("alert.fingerprint", alert.fingerprint)
    , ("alert.description", alert.description)
    , ("rule", context.mrcRuleName)
    , ("acked_by", fromMaybe "" context.mrcAckedBy)
    , ("acked_at", maybe "" formatStamp context.mrcAckedAt)
    , ("closed_by", fromMaybe "" context.mrcClosedBy)
    , ("ack_url", fromMaybe "" context.mrcAckUrl)
    , ("action_url", fromMaybe "" context.mrcActionUrl)
    , ("alert_url", context.mrcAlertUrl)
    , ("alert_id", context.mrcAlertId)
    , ("color", resolveColor colorOverrides alert)
    , ("line_ack", onlyStatus "ack" ackLine)
    , ("line_closed", onlyStatus "closed" closedLine)
    , ("line_resolved", onlyStatus "resolved" "Resolved by the source")
    , ("line_stalled", onlyStatus "stalled" "Stalled: no source updates")
    , ("line_firing", if statusText alert `elem` (["ack", "closed", "resolved", "stalled"] :: [Text]) then "" else firingLine)
    ]
  where
    onlyStatus status text = if statusText alert == status then text else ""
    ackLine =
        "Acked by "
            <> fromMaybe "unknown" context.mrcAckedBy
            <> maybe "" (" at " <>) (formatStamp <$> context.mrcAckedAt)
    closedLine = "Closed" <> maybe "" (" by " <>) context.mrcClosedBy
    firingLine =
        "Firing · "
            <> tshow alert.occurrences
            <> " occurrence(s)"
            <> maybe "" (\url -> " · [Ack](" <> url <> ")") context.mrcAckUrl

-- Render a template body and drop empty lines, so optional slots (e.g. an
-- absent description) do not leave blank lines behind.
renderTemplateLines :: Text -> [(Text, Text)] -> Text
renderTemplateLines body bindings =
    Text.intercalate "\n" (filter (not . Text.null) (Text.lines (renderTemplate body bindings)))

-- | Root post message line. The Maybe is the active mattermost_root template
-- body; Nothing falls back to defaultRootTemplateBody.
renderRootMessage :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> Text
renderRootMessage template colorOverrides context alert =
    renderTemplateLines (fromMaybe defaultRootTemplateBody template) (mmBindings colorOverrides context alert)

-- | Attachment status line (mattermost_status template; falls back to
-- defaultStatusTemplateBody).
renderStatusMessage :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> Text
renderStatusMessage template colorOverrides context alert =
    renderTemplateLines (fromMaybe defaultStatusTemplateBody template) (mmBindings colorOverrides context alert)

-- | Attachment fields grid (mattermost_fields template): rendered
-- "Title|value" lines become {title, value, short:true} entries; value-empty
-- lines are dropped. Falls back to defaultFieldsTemplateBody.
renderFields :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> [Value]
renderFields template colorOverrides context alert =
    map fieldPair (renderFieldPairs template colorOverrides context alert)

-- The fields grid as (title, value) pairs — the attachment JSON builder
-- (renderFields) and the admin page preview share this.
renderFieldPairs :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> [(Text, Text)]
renderFieldPairs template colorOverrides context alert =
    [ (title, value)
    | line <- Text.lines (renderTemplateLines (fromMaybe defaultFieldsTemplateBody template) bindings)
    , let (title, rest) = Text.break (== '|') line
    , let value = Text.drop 1 rest
    , not (Text.null value)
    ]
  where
    bindings = mmBindings colorOverrides context alert

-- | Attachment color bar (mattermost_color template): the first rendered
-- line that looks like a color wins (typically "{{color}}" referencing the
-- config mapping, or a literal "#RRGGBB"; MM also accepts the named colors
-- good/warning/danger). Anything else — empty, leftover {{slots}}, random
-- text — falls back to the direct mapping resolution, so a broken color
-- template degrades to the default bar instead of a broken attachment.
renderColor :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> Text
renderColor template colorOverrides context alert =
    case filter isUsableColor (map Text.strip (Text.lines rendered)) of
        (line : _) -> line
        [] -> resolveColor colorOverrides alert
  where
    rendered = renderTemplateLines (fromMaybe defaultColorTemplateBody template) (mmBindings colorOverrides context alert)
    isUsableColor line = case Text.uncons line of
        Just ('#', hex) -> not (Text.null hex) && Text.all (`elem` ("0123456789abcdefABCDEF" :: String)) hex
        _ -> Text.toLower line `elem` (["good", "warning", "danger"] :: [Text])

-- | Root post props: one attachment with fields, color, and (for firing
-- alerts) the Ack action wired back to Halemans. The Maybes are the active
-- mattermost_status / mattermost_color / mattermost_fields template bodies;
-- the color overrides come from the channel config (colorMapFromJson).
renderRootProps :: Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> Value
renderRootProps statusTemplate colorTemplate fieldsTemplate colorOverrides context alert =
    object
        [ "attachments"
            .= [ object
                    [ "color" .= renderColor colorTemplate colorOverrides context alert
                    , "text" .= renderStatusMessage statusTemplate colorOverrides context alert
                    , "fields" .= renderFields fieldsTemplate colorOverrides context alert
                    , "actions" .= ackAction context alert
                    ]
               ]
        ]

-- | Thread-reply message with the full alert details. The Maybe is the
-- active mattermost_details template body; Nothing falls back to
-- defaultDetailsTemplateBody.
renderDetailsMessage :: Maybe Text -> [(Text, Text)] -> MattermostRenderContext -> Alert -> Text
renderDetailsMessage template colorOverrides context alert =
    renderTemplateLines (fromMaybe defaultDetailsTemplateBody template) (mmBindings colorOverrides context alert)

statusText :: Alert -> Text
statusText alert = alert.status

-- UTC stamp for the root status line; the post is re-rendered server-side,
-- so no per-user timezone is available here.
formatStamp :: UTCTime -> Text
formatStamp = cs . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"

stateLabel :: Text -> Text
stateLabel status = case status of
    "firing" -> "FIRING"
    "ack" -> "ACKED"
    "resolved" -> "RESOLVED"
    "stalled" -> "STALLED"
    "closed" -> "CLOSED"
    _ -> Text.toUpper status

fieldOrDash :: AlertField -> Alert -> Text
fieldOrDash field alert = fromMaybe "-" (effectiveFieldText field alert)

fieldPair :: (Text, Text) -> Value
fieldPair (title, value) = object ["title" .= title, "value" .= value, "short" .= True]

ackAction :: MattermostRenderContext -> Alert -> [Value]
ackAction context alert =
    case (statusText alert, context.mrcActionUrl) of
        ("firing", Just actionUrl) ->
            [ object
                [ "id" .= ("ack" :: Text)
                , "name" .= ("Ack" :: Text)
                , "integration" .= object ["url" .= actionUrl, "context" .= object ["alertId" .= context.mrcAlertId]]
                ]
            ]
        _ -> []
