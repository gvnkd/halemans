module Application.Job.Mattermost (
    enqueueNotify,
    enqueueSyncIfPosted,
) where

import Application.Service.Mattermost (deliverNotify, syncAlertPosts)
import qualified Application.Service.Mattermost.Render as Render
import Control.Monad (void)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.Job.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

-- Outbound Mattermost delivery. Ingest and the alert pipeline only enqueue;
-- the worker owns the HTTP so a slow Mattermost server never blocks
-- ingestion. kind 'notify' = initial root post + details thread (the
-- service de-duplicates onto a plain sync when the pair already has a
-- post); kind 'sync' = re-render + patch existing root posts.

instance Job MattermostJob where
    perform job = do
        alert <- fetch job.alertId
        case job.kind of
            "notify" -> case job.ruleId of
                Nothing -> pure ()
                Just ruleId -> do
                    rule <- fetch ruleId
                    result <- deliverNotify alert rule
                    case result of
                        Right () -> pure ()
                        Left err -> error (cs err) -- job retry/backoff
            _ -> syncAlertPosts alert

    maxAttempts = 5

-- | Fired by Notify.fireRule for rules whose channel is mattermost.
enqueueNotify :: (?modelContext :: ModelContext) => Alert -> NotificationRule -> IO ()
enqueueNotify alert rule =
    void do
        newRecord @MattermostJob
            |> set #alertId (get #id alert)
            |> set #ruleId (Just (get #id rule))
            |> set #kind ("notify" :: Text)
            |> createRecord

-- | Called from the ingest fan-out (publishAlertUpdate) on every alert
-- transition: only state-changing kinds sync, and only when the alert
-- actually has Mattermost posts to patch.
enqueueSyncIfPosted :: (?modelContext :: ModelContext) => Alert -> Text -> IO ()
enqueueSyncIfPosted alert eventKind =
    when (Render.syncsKind eventKind) do
        posted <-
            query @MattermostPost
                |> filterWhere (#alertId, get #id alert)
                |> fetch
        unless (null posted) do
            _ <-
                newRecord @MattermostJob
                    |> set #alertId (get #id alert)
                    |> set #kind ("sync" :: Text)
                    |> set #eventKind (Just eventKind)
                    |> createRecord
            pure ()
