module Application.Service.Mattermost.Render (
    MattermostRenderContext (..),
    renderRootMessage,
    renderRootProps,
    renderDetailsMessage,
    syncsKind,
    defaultRootTemplateBody,
    defaultDetailsTemplateBody,
    mattermostRootTemplateName,
    mattermostDetailsTemplateName,
    mattermostTemplateNames,
    mattermostSlotNames,
) where

import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Application.Service.Llm.Prompt (renderTemplate)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Generated.Types
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

mattermostTemplateNames :: [Text]
mattermostTemplateNames = [mattermostRootTemplateName, mattermostDetailsTemplateName]

defaultRootTemplateBody :: Text
defaultRootTemplateBody = "[{{alert.state}}] {{alert.title}}"

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
mattermostSlotNames = map fst (mmBindings stubContext stubAlert)
  where
    stubContext =
        MattermostRenderContext
            { mrcRuleName = ""
            , mrcAckedBy = Nothing
            , mrcAckedAt = Nothing
            , mrcClosedBy = Nothing
            , mrcActionUrl = Nothing
            , mrcAckUrl = Nothing
            , mrcAlertUrl = ""
            , mrcAlertId = ""
            }
    stubAlert = newRecord @Alert

-- | Bindings shared by the root and details templates. Alert fields use the
-- alert.* prefix (the LLM template convention); the render-context values are
-- plain names.
mmBindings :: MattermostRenderContext -> Alert -> [(Text, Text)]
mmBindings context alert =
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
    ]

-- Render a template body and drop empty lines, so optional slots (e.g. an
-- absent description) do not leave blank lines behind.
renderTemplateLines :: Text -> [(Text, Text)] -> Text
renderTemplateLines body bindings =
    Text.intercalate "\n" (filter (not . Text.null) (Text.lines (renderTemplate body bindings)))

-- | Root post message line. The Maybe is the active mattermost_root template
-- body; Nothing falls back to defaultRootTemplateBody.
renderRootMessage :: Maybe Text -> MattermostRenderContext -> Alert -> Text
renderRootMessage template context alert =
    renderTemplateLines (fromMaybe defaultRootTemplateBody template) (mmBindings context alert)

-- | Root post props: one attachment with fields, color, and (for firing
-- alerts) the Ack action wired back to Halemans.
renderRootProps :: MattermostRenderContext -> Alert -> Value
renderRootProps context alert =
    object
        [ "attachments"
            .= [ object
                    [ "color" .= stateColor alert
                    , "text" .= statusLine context alert
                    , "fields" .= map fieldPair (rootFields context alert)
                    , "actions" .= ackAction context alert
                    ]
               ]
        ]

-- | Thread-reply message with the full alert details. The Maybe is the
-- active mattermost_details template body; Nothing falls back to
-- defaultDetailsTemplateBody.
renderDetailsMessage :: Maybe Text -> MattermostRenderContext -> Alert -> Text
renderDetailsMessage template context alert =
    renderTemplateLines (fromMaybe defaultDetailsTemplateBody template) (mmBindings context alert)

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

stateColor :: Alert -> Text
stateColor alert = case statusText alert of
    "resolved" -> "#98A2AD"
    "closed" -> "#98A2AD"
    "stalled" -> "#98A2AD"
    _ -> case alert.severity of
        "critical" -> "#E5484D"
        "high" -> "#FF5A1F"
        "warning" -> "#F7B500"
        _ -> "#4C8DFF"

statusLine :: MattermostRenderContext -> Alert -> Text
statusLine context alert = case statusText alert of
    "ack" ->
        "Acked by "
            <> fromMaybe "unknown" context.mrcAckedBy
            <> maybe "" (" at " <>) (formatStamp <$> context.mrcAckedAt)
    "closed" -> "Closed" <> maybe "" (" by " <>) context.mrcClosedBy
    "resolved" -> "Resolved by the source"
    "stalled" -> "Stalled: no source updates"
    _ ->
        "Firing · "
            <> tshow alert.occurrences
            <> " occurrence(s)"
            <> maybe "" (\url -> " · [Ack](" <> url <> ")") context.mrcAckUrl

rootFields :: MattermostRenderContext -> Alert -> [(Text, Text)]
rootFields context alert =
    [ ("Environment", fieldOrDash FieldEnv alert)
    , ("Host", fieldOrDash FieldHost alert)
    , ("Service", fieldOrDash FieldService alert)
    , ("Severity", alert.severity)
    , ("Rule", context.mrcRuleName)
    ]

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
