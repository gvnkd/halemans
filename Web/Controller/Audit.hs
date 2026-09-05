module Web.Controller.Audit where

import Web.Controller.Prelude
import Web.View.Audit.Index
import Application.Service.AuditExport (ExportRow (..), renderCsv, renderJsonl, iso8601)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Network.HTTP.Types (status200, status400)
import Network.Wai (responseLBS)
import IHP.ControllerSupport (respondAndExit)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.UUID as UUID

-- Audit export (design_docs/milestone_5.md §5): admin-gated download of
-- alert_events as CSV/JSONL. Every request lands an audit_exports row before
-- streaming; the exported bytes are not stored.
instance Controller AuditController where
    beforeAction = ensureIsUser

    action AuditExportsAction = do
        requirePrivilege "admin"
        exports <- query @AuditExport
            |> orderByDesc #createdAt
            |> limit 50
            |> fetch
        users <- query @User |> fetch
        render IndexView { .. }

    action ExportAuditAction = do
        requirePrivilege "admin"
        now <- getCurrentTime
        let fromParam = paramOrNothing @Text "from"
            toParam = paramOrNothing @Text "to"
            envParam = nonEmptyText (paramOrNothing @Text "environment")
            alertParam = nonEmptyText (paramOrNothing @Text "alert") >>= UUID.fromText
            alertInvalid = isJust (nonEmptyText (paramOrNothing @Text "alert")) && isNothing alertParam
            formatParam = fromMaybe "csv" (nonEmptyText (paramOrNothing @Text "format"))
            from = fromMaybe (addUTCTime (-7 * 86400) now) (fromParam >>= parseTimestamp)
            to = fromMaybe now (toParam >>= parseTimestamp)
        case (formatParam `elem` ["csv", "jsonl"], isJust envParam && isJust alertParam || alertInvalid) of
            (False, _) -> respondAndExit $ responseLBS status400 [("Content-Type", "text/plain; charset=utf-8")] "format must be csv or jsonl"
            (_, True) -> respondAndExit $ responseLBS status400 [("Content-Type", "text/plain; charset=utf-8")] "scope accepts at most one of environment or alert (valid uuid)"
            (True, False) -> do
                let fromText = iso8601 from
                    toText = iso8601 to
                rows <- case (envParam, alertParam) of
                    (Just env, Nothing) -> sqlQueryTyped [typedSql|
                        SELECT e.id, e.created_at, e.kind, e.user_id, e.payload, e.alert_id, a.title, a.env
                        FROM alert_events e JOIN alerts a ON a.id = e.alert_id
                        WHERE e.created_at >= ${from}::timestamptz AND e.created_at < ${to}::timestamptz
                            AND a.env = ${env}
                        ORDER BY e.created_at |]
                    (Nothing, Just alertId) -> sqlQueryTyped [typedSql|
                        SELECT e.id, e.created_at, e.kind, e.user_id, e.payload, e.alert_id, a.title, a.env
                        FROM alert_events e JOIN alerts a ON a.id = e.alert_id
                        WHERE e.created_at >= ${from}::timestamptz AND e.created_at < ${to}::timestamptz
                            AND e.alert_id = ${alertId}
                        ORDER BY e.created_at |]
                    _ -> sqlQueryTyped [typedSql|
                        SELECT e.id, e.created_at, e.kind, e.user_id, e.payload, e.alert_id, a.title, a.env
                        FROM alert_events e JOIN alerts a ON a.id = e.alert_id
                        WHERE e.created_at >= ${from}::timestamptz AND e.created_at < ${to}::timestamptz
                        ORDER BY e.created_at |]
                let exportRows = map toExportRow rows
                let scope = object
                        [ "from" .= fromText
                        , "to" .= toText
                        , "environment" .= envParam
                        , "alert" .= alertParam
                        ]
                export <- newRecord @AuditExport
                    |> set #userId (Just currentUserId)
                    |> set #scope scope
                    |> set #format formatParam
                    |> createRecord
                let body = if formatParam == "csv" then renderCsv exportRows else renderJsonl exportRows
                _ <- export
                    |> set #rowCount (length exportRows)
                    |> updateRecord
                let extension :: Text
                    extension = if formatParam == "csv" then "csv" else "jsonl"
                    contentType :: Text
                    contentType = if formatParam == "csv" then "text/csv; charset=utf-8" else "application/x-ndjson; charset=utf-8"
                    filename :: Text
                    filename = "halemans-audit-" <> cs (formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now) <> "." <> extension
                respondAndExit $ responseLBS status200
                    [ ("Content-Type", cs contentType)
                    , ("Content-Disposition", cs ("attachment; filename=\"" <> filename <> "\""))
                    ]
                    (cs body)
      where
        toExportRow row = ExportRow
            { eventId = tshow (get #id row)
            , eventCreatedAt = get #created_at row
            , alertId = tshow (get #alert_id row)
            , alertTitle = get #title row
            , alertEnv = get #env row
            , kind = get #kind row
            , userId = tshow <$> get #user_id row
            , payload = get #payload row
            }

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText = maybe Nothing (\value -> if value == "" then Nothing else Just value)

parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp value =
    tryFormat "%Y-%m-%dT%H:%M:%SZ"
        <|> tryFormat "%Y-%m-%dT%H:%M:%S%z"
        <|> tryFormat "%Y-%m-%d"
  where
    tryFormat fmt = parseTimeM True defaultTimeLocale fmt (cs value)
