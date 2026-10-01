module Application.Service.Mattermost.Render (
    MattermostRenderContext (..),
    renderRootMessage,
    renderRootProps,
    renderDetailsMessage,
    syncsKind,
) where

import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Generated.Types
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
    , mrcAlertUrl :: Text
    , mrcAlertId :: Text
    }
    deriving (Eq, Show)

-- | AlertEvent kinds worth syncing to the root post. "created" is excluded
-- on purpose: the initial root post is the notify job's job; "comment",
-- "writeback", ... never change the visual state.
syncsKind :: Text -> Bool
syncsKind kind = kind `elem` (["repeated", "resolved", "ack", "unack", "closed", "stalled"] :: [Text])

-- | Root post message line: [STATE] title, state in caps.
renderRootMessage :: Alert -> Text
renderRootMessage alert = "[" <> stateLabel (statusText alert) <> "] " <> alert.title

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

-- | Thread-reply message with the full alert details.
renderDetailsMessage :: MattermostRenderContext -> Alert -> Text
renderDetailsMessage context alert =
    Text.intercalate
        "\n"
        ( filter
            (not . Text.null)
            [ "**" <> alert.title <> "**"
            , alert.description
            , ""
            , "Severity: " <> alert.severity
            , "Status: " <> statusText alert
            , "Environment: " <> fieldOrDash FieldEnv alert
            , "Host: " <> fieldOrDash FieldHost alert
            , "Service: " <> fieldOrDash FieldService alert
            , "Occurrences: " <> tshow alert.occurrences
            , "Fingerprint: `" <> alert.fingerprint <> "`"
            , ""
            , "[Open in Halemans](" <> context.mrcAlertUrl <> ")"
            ]
        )

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
    _ -> "Firing · " <> tshow alert.occurrences <> " occurrence(s)"

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
