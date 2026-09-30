module Application.Service.Mattermost.Actions (ackFromMattermost) where

import Application.Pipeline.Actions (ackAlert)
import Application.Service.Mattermost (syncAlertPosts)
import Generated.Types
import IHP.Fetch (fetchOneOrNothing)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- The Ack button action. Kept in its own module because it imports the alert
-- pipeline (ackAlert → ingest fan-out → Mattermost job → Service.Mattermost);
-- Service.Mattermost itself must not import the pipeline or the cycle
-- closes. The web hook controller is a thin wrapper over
-- ackFromMattermost.

mattermostEmail :: Text
mattermostEmail = "mattermost@localhost"

-- | Resolve the clicker (Mattermost username matches a Halemans display
-- name, else the mattermost@localhost service account), ack the alert, then
-- refresh the root posts so the button state follows. Returns the ephemeral
-- reply text Mattermost shows the clicker.
ackFromMattermost :: (?modelContext :: ModelContext) => Id Alert -> Text -> IO (Either Text Text)
ackFromMattermost alertId username = do
    alertOrNothing <- fetchOneOrNothing alertId
    case alertOrNothing of
        Nothing -> pure (Left "alert not found")
        Just alert -> do
            actor <- resolveActor username
            _ <- ackAlert actor alert (Just "acked via Mattermost") Nothing
            syncAlertPosts alert
            pure (Right ("Acked by " <> actor.displayName))

resolveActor :: (?modelContext :: ModelContext) => Text -> IO User
resolveActor username = do
    byName <- query @User |> filterWhere (#displayName, username) |> fetchOneOrNothing
    case byName of
        Just user -> pure user
        Nothing -> ensureServiceUser

ensureServiceUser :: (?modelContext :: ModelContext) => IO User
ensureServiceUser = do
    existing <- query @User |> filterWhere (#email, mattermostEmail) |> fetchOneOrNothing
    case existing of
        Just user -> pure user
        Nothing ->
            newRecord @User
                |> set #email mattermostEmail
                |> set #displayName "Mattermost"
                |> set #passwordHash "!"
                |> createRecord
