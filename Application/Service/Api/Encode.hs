module Application.Service.Api.Encode
    ( encodeAlertSummary
    , encodeAlertDetail
    , encodeEnvCard
    ) where

import IHP.Prelude
import Generated.Types
import Data.Aeson (Value, object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import IHP.ModelSupport (Id' (..), PrimaryKey)
import Web.View.Dashboard.Index (EnvCard (..))
import Application.Service.Api.Alerts (AlertDetail (..))
import Application.Pipeline.Grouping (AlertField (..), effectiveFieldText)

-- Explicit encoder functions (design_docs/milestone_6.md §3): no ToJSON
-- instances on Generated types so the wire shape survives schema drift.
-- Field naming is snake_case, timestamps ISO-8601 UTC via aeson's UTCTime
-- encoding.

idValue :: PrimaryKey table ~ UUID => Id' table -> Value
idValue (Id uuid) = toJSON (UUID.toText uuid)

maybeIdValue :: PrimaryKey table ~ UUID => Maybe (Id' table) -> Value
maybeIdValue = maybe Aeson.Null idValue

encodeAlertSummary :: Alert -> Value
encodeAlertSummary alert = object
    [ "id" .= idValue (get #id alert)
    , "fingerprint" .= alert.fingerprint
    , "title" .= alert.title
    , "description" .= alert.description
    , "severity" .= alert.severity
    , "status" .= alert.status
    , "env" .= effectiveFieldText FieldEnv alert
    , "host" .= effectiveFieldText FieldHost alert
    , "service" .= effectiveFieldText FieldService alert
    , "check_name" .= alert.checkName
    , "facets" .= alert.facets
    , "source_id" .= maybeIdValue alert.sourceId
    , "external_id" .= alert.externalId
    , "labels" .= alert.labels
    , "annotations" .= alert.annotations
    , "source_url" .= alert.sourceUrl
    , "occurrences" .= alert.occurrences
    , "started_at" .= alert.startedAt
    , "first_seen_at" .= alert.firstSeenAt
    , "last_seen_at" .= alert.lastSeenAt
    , "resolved_at" .= alert.resolvedAt
    , "environment_id" .= maybeIdValue alert.environmentId
    , "host_id" .= maybeIdValue alert.hostId
    , "service_id" .= maybeIdValue alert.serviceId
    , "suppressed" .= alert.suppressed
    , "acknowledged_by" .= maybeIdValue alert.acknowledgedBy
    , "acknowledged_at" .= alert.acknowledgedAt
    , "ack_comment" .= alert.ackComment
    , "ack_expires_at" .= alert.ackExpiresAt
    , "closed_by" .= maybeIdValue alert.closedBy
    , "closed_at" .= alert.closedAt
    , "close_reason" .= alert.closeReason
    , "group_id" .= maybeIdValue alert.groupId
    , "created_at" .= alert.createdAt
    , "updated_at" .= alert.updatedAt
    ]

encodeAlertDetail :: AlertDetail -> Value
encodeAlertDetail AlertDetail { .. } = object
    [ "alert" .= encodeAlertSummary adAlert
    , "environment" .= maybe Aeson.Null (\e -> object ["id" .= idValue (get #id e), "name" .= get #name e]) adEnvironment
    , "host" .= maybe Aeson.Null (\h -> object ["id" .= idValue (get #id h), "name" .= get #fqdn h]) adHost
    , "service" .= maybe Aeson.Null (\s -> object ["id" .= idValue (get #id s), "name" .= get #name s]) adService
    , "group" .= maybe Aeson.Null (\g -> object ["id" .= idValue (get #id g), "title" .= get #title g]) adGroup
    , "jira_links" .= map encodeJiraLink adJiraLinks
    , "cmdb" .= maybe Aeson.Null encodeCmdbEntry adCmdb
    , "llm_analysis" .= maybe Aeson.Null encodeLlmAnalysis adAnalysis
    , "timeline" .= map encodeTimelineEvent adTimeline
    ]

encodeJiraLink :: JiraLink -> Value
encodeJiraLink link = object
    [ "ticket_key" .= link.ticketKey
    , "summary" .= link.summary
    , "status" .= link.status
    , "url" .= link.url
    , "origin" .= link.origin
    , "synced_at" .= link.syncedAt
    ]

encodeCmdbEntry :: CmdbEntry -> Value
encodeCmdbEntry entry = object
    [ "title" .= entry.title
    , "excerpt" .= entry.excerpt
    , "url" .= entry.url
    , "fetched_at" .= entry.fetchedAt
    ]

encodeLlmAnalysis :: LlmAnalysis -> Value
encodeLlmAnalysis analysis = object
    [ "id" .= idValue (get #id analysis)
    , "provider" .= analysis.provider
    , "model" .= analysis.model
    , "prompt_version" .= analysis.promptVersion
    , "markdown" .= analysis.resultMd
    , "result" .= analysis.result
    , "tokens_in" .= analysis.tokensIn
    , "tokens_out" .= analysis.tokensOut
    , "created_at" .= analysis.createdAt
    ]

encodeTimelineEvent :: (AlertEvent, Maybe Text) -> Value
encodeTimelineEvent (event, userEmail) = object
    [ "id" .= idValue (get #id event)
    , "kind" .= event.kind
    , "payload" .= event.payload
    , "user_id" .= maybeIdValue event.userId
    , "user_email" .= userEmail
    , "created_at" .= event.createdAt
    ]

-- The overview-dashboard rollup as JSON (design §3): computed by the same
-- query module the dashboard uses, so the numbers agree by construction.
encodeEnvCard :: EnvCard -> Value
encodeEnvCard card = object
    [ "environment" .= maybe Aeson.Null (\name -> object ["id" .= maybeIdValue (get #id <$> card.cardEnvironment), "name" .= name]) card.cardEnvName
    , "worst_severity" .= card.cardWorstSeverity
    , "counts" .= object
        [ "firing" .= card.cardFiring
        , "ack" .= card.cardAcked
        , "resolved" .= card.cardResolved
        ]
    , "suppressed" .= card.cardSuppressed
    ]
