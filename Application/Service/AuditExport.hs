module Application.Service.AuditExport where

import IHP.Prelude
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Format (formatTime, defaultTimeLocale)

-- Audit export (design_docs/milestone_5.md §5): CSV/JSONL serialization of
-- alert_events joined to their alert. Pure renderers for round-trip unit
-- tests; the controller fetches rows and records the export.

data ExportRow = ExportRow
    { eventId :: Text
    , eventCreatedAt :: UTCTime
    , alertId :: Text
    , alertTitle :: Text
    , alertEnv :: Maybe Text
    , kind :: Text
    , userId :: Maybe Text
    , payload :: Value
    }

csvHeader :: Text
csvHeader = "event_id,created_at,alert_id,alert_title,environment,kind,user_id,payload"

renderCsv :: [ExportRow] -> Text
renderCsv rows = Text.concat (map (<> "\n") (csvHeader : map csvRow rows))

csvRow :: ExportRow -> Text
csvRow row = Text.intercalate ","
    [ csvField row.eventId
    , csvField (iso8601 row.eventCreatedAt)
    , csvField row.alertId
    , csvField row.alertTitle
    , csvField (fromMaybe "" row.alertEnv)
    , csvField row.kind
    , csvField (fromMaybe "" row.userId)
    , csvField (cs (Aeson.encode row.payload))
    ]

csvField :: Text -> Text
csvField value
    | Text.any (\c -> c == ',' || c == '"' || c == '\n' || c == '\r') value =
        "\"" <> Text.intercalate "\"\"" (Text.splitOn "\"" value) <> "\""
    | otherwise = value

renderJsonl :: [ExportRow] -> Text
renderJsonl rows = Text.concat (map (<> "\n") (map jsonlRow rows))

jsonlRow :: ExportRow -> Text
jsonlRow row = cs (Aeson.encode (Aeson.object
    [ "event_id" Aeson..= row.eventId
    , "created_at" Aeson..= iso8601 row.eventCreatedAt
    , "alert_id" Aeson..= row.alertId
    , "alert_title" Aeson..= row.alertTitle
    , "environment" Aeson..= row.alertEnv
    , "kind" Aeson..= row.kind
    , "user_id" Aeson..= row.userId
    , "payload" Aeson..= row.payload
    ]))

iso8601 :: UTCTime -> Text
iso8601 = cs . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
