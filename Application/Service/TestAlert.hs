module Application.Service.TestAlert (
    fireTestAlert,
) where

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

-- Fire a synthetic test alert through the REAL ingest pipeline so operators
-- can exercise notification channels, escalation policies, grouping and
-- blackouts exactly as production events do. The alert is distinguishable
-- ([TEST] title prefix, fingerprint "test:<source>:<uuid>") and every click
-- gets a fresh fingerprint, so it never dedupes into an earlier test alert
-- and rule throttles (keyed on the fingerprint) don't suppress it.

fireTestAlert :: (?modelContext :: ModelContext) => Source -> IO (Id Alert)
fireTestAlert source = do
    uuid <- tshow <$> nextRandom
    let fingerprint = "test:" <> tshow (get #id source) <> ":" <> uuid
        event =
            NormalizedEvent
                { fingerprint
                , externalId = Nothing
                , status = Firing
                , severity = "warning"
                , title = "[TEST] " <> source.name <> " test alert"
                , description = "Synthetic test alert fired from Admin → Sources to test notification channels and escalation policies."
                , env = if Text.null source.env then Nothing else Just source.env
                , host = Nothing
                , service = Nothing
                , checkName = Nothing
                , labels = Aeson.object []
                , annotations = Aeson.object []
                , hostGroups = []
                , startedAt = Nothing
                , sourceUrl = Nothing
                }
    maybeAlertId <- ingest source event
    case maybeAlertId of
        Just alertId -> pure alertId
        Nothing -> error "test alert ingest returned no alert (unexpected)"
