module Application.Service.Defaults (ensureDefaults) where

import Application.Service.Agent.Core (defaultAgentTemplateBody, internalAgentTemplateName)
import Application.Service.Jira.Related (defaultJiraRelatedTemplateBody, relatedTemplateName)
import Application.Service.Llm.Prompt (defaultEnrichmentTemplateBody, enrichmentTemplateName)
import Application.Service.Mattermost.Render (
    defaultTemplateBodyFor,
    mattermostAttachmentTemplateName,
    mattermostColorTemplateName,
    mattermostDetailsTemplateName,
    mattermostFieldsTemplateName,
    mattermostRootTemplateName,
    mattermostStatusTemplateName,
 )
import Control.Monad (void)
import Generated.Types
import IHP.ModelSupport (ModelContext, withTransaction)
import IHP.Prelude
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)

-- Boot-time default provisioning: INSERT-if-missing for the built-in rows
-- that previously existed only in Application/Fixtures.sql (fresh DBs) and
-- nix/scripts/seed-halemans.sh (dev/smoke), so long-lived DBs get them too.
--
-- Policy: existing rows are NEVER touched — no updates, no re-activation,
-- no privilege re-assertion. A provision config file applied afterwards
-- still wins. Idempotent under concurrent boots (web + worker both run the
-- configIO hook): everything runs in one advisory-locked transaction and
-- every insert is conflict-guarded (UNIQUE (name, version) on
-- llm_prompt_templates serializes racing template inserts; ON CONFLICT
-- DO NOTHING covers the rest).
--
-- Bodies come from the same code constants as the runtime fallbacks
-- (Render.hs, Agent/Core.hs, Related.hs, Llm/Prompt.hs) so the seeded rows
-- and the built-in fallbacks can never drift apart.

ensureDefaults :: (?modelContext :: ModelContext) => IO ()
ensureDefaults = withTransaction do
    let lockKey = "halemans:defaults" :: Text
    void
        ( sqlQueryTyped
            [typedSql|
        SELECT 1 WHERE pg_advisory_xact_lock(hashtextextended(${lockKey}, 0)) IS NULL
    |] ::
            IO [Int]
        )
    forM_ defaultTemplates ensureTemplateVersion1
    forM_ defaultChannels ensureChannel
    ensureRetentionConfig
    forM_ defaultRoles ensureRole

defaultTemplates :: [(Text, Text)]
defaultTemplates =
    [ (name, body)
    | name <-
        [ mattermostRootTemplateName
        , mattermostDetailsTemplateName
        , mattermostStatusTemplateName
        , mattermostFieldsTemplateName
        , mattermostColorTemplateName
        , mattermostAttachmentTemplateName
        ]
    , Just body <- [defaultTemplateBodyFor name]
    ]
        ++ [ (internalAgentTemplateName, defaultAgentTemplateBody)
           , (relatedTemplateName, defaultJiraRelatedTemplateBody)
           , (enrichmentTemplateName, defaultEnrichmentTemplateBody)
           ]

-- Any row carrying the name — a user edit, a provisioned row, a seed-script
-- version chain — suppresses the insert.
ensureTemplateVersion1 :: (?modelContext :: ModelContext) => (Text, Text) -> IO ()
ensureTemplateVersion1 (name, body) = void do
    sqlExecTyped
        [typedSql|
        INSERT INTO llm_prompt_templates (name, version, body, active, notes)
        SELECT ${name}, 1, ${body}, true, 'built-in default'
        WHERE NOT EXISTS (SELECT 1 FROM llm_prompt_templates WHERE name = ${name})
        ON CONFLICT (name, version) DO NOTHING
    |]

defaultChannels :: [(Text, Text)]
defaultChannels =
    [ ("browser_push", "browser_push")
    , ("email", "email")
    ]

ensureChannel :: (?modelContext :: ModelContext) => (Text, Text) -> IO ()
ensureChannel (name, channelType) = void do
    sqlExecTyped
        [typedSql|
        INSERT INTO notification_channels (name, type)
        VALUES (${name}, ${channelType})
        ON CONFLICT (name) DO NOTHING
    |]

ensureRetentionConfig :: (?modelContext :: ModelContext) => IO ()
ensureRetentionConfig = void do
    sqlExecTyped
        [typedSql|
        INSERT INTO retention_configs (raw_events_days, enabled)
        SELECT 30, true
        WHERE NOT EXISTS (SELECT 1 FROM retention_configs)
    |]

defaultRoles :: [(Text, [Text])]
defaultRoles =
    [ ("admin", ["view", "ack", "close", "escalate", "manage_blackouts", "manage_rules", "manage_users", "manage_sources", "admin"])
    , ("sre", ["view", "ack", "close", "escalate"])
    , ("viewer", ["view"])
    ]

ensureRole :: (?modelContext :: ModelContext) => (Text, [Text]) -> IO ()
ensureRole (name, privileges) = void do
    sqlExecTyped
        [typedSql|
        INSERT INTO roles (name, privileges)
        VALUES (${name}, ${privileges})
        ON CONFLICT (name) DO NOTHING
    |]
