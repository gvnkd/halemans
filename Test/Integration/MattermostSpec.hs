module Test.Integration.MattermostSpec (spec) where

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Job.Mattermost ()
import Application.Pipeline.Actions (ackAlert)
import Application.Service.Mattermost.Actions (ackFromMattermost)
import Application.Service.Provision (ProvisionError (..))
import Application.Service.TestAlert (fireTestAlert)
import Control.Exception (finally, try)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.Fetch (fetch, fetchOne)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types (Job (..))
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, typedSql)
import System.Environment (lookupEnv, setEnv)
import System.Process (readProcess)
import Test.Hspec
import Test.Integration.Setup (exposeFor, freshFingerprint, integrationSource, m7Apply, restoreEnv, testEventIn, testSource, testUser)

-- Mattermost channel end-to-end against the mock (nix/mocks/mock_mattermost.py,
-- started by checks.nix on 18088; focused local runs start it manually with
-- MATTERMOST_TOKEN=test-mattermost-token). Runs the MattermostJob rows
-- inline, like the other integration specs do for their jobs.

spec :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Spec
spec = describe "Mattermost notification channel" do
    it "delivers root post + details thread, then syncs the root on ack" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-itest-env-" <> suffix
        withMattermostEnv do
            mockReset
            source <- testSource
            _ <- mattermostRule ("mm-itest-rule-" <> suffix) ("alerts-itest-" <> suffix) envName
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            alert <- fetch alertId
            alert.status `shouldBe` "firing"

            notifyJobs <-
                query @MattermostJob
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "notify" :: Text)
                    |> fetch
            notifyJob <- expectOne notifyJobs
            perform notifyJob

            posts <- mockPosts
            (root, reply) <- expectRootAndReply posts
            postMessage root `shouldBe` "[FIRING] integration test alert"
            postRootId reply `shouldBe` postId root
            postMessage reply `shouldSatisfy` (Text.isInfixOf "integration test alert")
            actionNames root `shouldBe` ["Ack"]
            actionUrlFor "Ack" root `shouldSatisfy` (Text.isInfixOf "/hooks/mattermost/actions/")

            posted <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch
            posted.rootPostId `shouldBe` postId root

            user <- testUser
            _ <- ackAlert user alert Nothing Nothing
            syncJobs <-
                query @MattermostJob
                    |> filterWhere (#alertId, alertId)
                    |> filterWhere (#kind, "sync" :: Text)
                    |> fetch
            syncJob <- expectOne syncJobs
            perform syncJob

            postsAfterAck <- mockPosts
            (rootAfterAck, _) <- expectRootAndReply postsAfterAck
            postMessage rootAfterAck `shouldBe` "[ACKED] integration test alert"
            actionNames rootAfterAck `shouldBe` []

    it "renders the root card sub-parts from the status/fields/color templates and the channel colors config" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-tpl-env-" <> suffix
            ruleName = "mm-tpl-rule-" <> suffix
            channelName = "mm-tpl-chan-" <> suffix
        withMattermostEnv do
            ( do
                    mockReset
                    deleteMattermostSubTemplates
                    _ <- newRecord @LlmPromptTemplate |> set #name "mattermost_status" |> set #version 1 |> set #body "{{alert.state}} x{{alert.occurrences}} via {{rule}}" |> set #active True |> createRecord
                    _ <- newRecord @LlmPromptTemplate |> set #name "mattermost_fields" |> set #version 1 |> set #body "Env|{{alert.env}}\nSev|{{alert.severity}}" |> set #active True |> createRecord
                    _ <- newRecord @LlmPromptTemplate |> set #name "mattermost_color" |> set #version 1 |> set #body "{{color}}" |> set #active True |> createRecord
                    rule <- mattermostRuleWithColors ruleName channelName envName (Aeson.object ["warning" Aeson..= Aeson.String "#0A0B0C"])
                    _ <- pure rule
                    source <- testSource
                    fp <- freshFingerprint
                    Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
                    exposeFor alertId
                    notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
                    perform notifyJob

                    posts <- mockPosts
                    (root, _) <- expectRootAndReply posts
                    attachmentFieldText "text" root `shouldBe` ("FIRING x1 via " <> ruleName)
                    attachmentFieldText "color" root `shouldBe` "#0A0B0C"
                    attachmentPairs root `shouldBe` [("Env", envName), ("Sev", "warning")]
                )
                `finally` deleteMattermostSubTemplates

    it "a second notify for the same alert does not duplicate the channel post" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-itest-env-dup-" <> suffix
        withMattermostEnv do
            mockReset
            source <- testSource
            _ <- mattermostRule ("mm-itest-rule-dup-" <> suffix) ("alerts-itest-dup-" <> suffix) envName
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            jobs <- query @MattermostJob |> filterWhere (#alertId, alertId) |> fetch
            notifyJob <- expectOne jobs
            perform notifyJob
            void (ingest source ((testEventIn envName fp Firing){severity = "warning"}))
            jobsAgain <- query @MattermostJob |> filterWhere (#alertId, alertId) |> fetch
            case reverse jobsAgain of
                (newest : _) -> perform newest
                [] -> expectationFailure "expected a mattermost job"
            posts <- mockPosts
            length posts `shouldBe` 2 -- still just root + details
    it "a rule without channelConfig posts to its team's mattermost channel" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-team-env-" <> suffix
            channelRowName = "mm-team-chanrow-" <> suffix
            teamChannelName = "alerts-team-itest-" <> suffix
        withMattermostEnv do
            mockReset
            source <- testSource
            team <-
                newRecord @Team
                    |> set #name ("mm-team-" <> suffix)
                    |> set #description ""
                    |> set
                        #defaults
                        ( Aeson.object
                            [ "mattermost"
                                Aeson..= Aeson.object
                                    [ "team" Aeson..= ("mock" :: Text)
                                    , "channel" Aeson..= teamChannelName
                                    ]
                            ]
                        )
                    |> createRecord
            rule <- mattermostRule ("mm-team-rule-" <> suffix) channelRowName envName
            _ <- rule |> set #teamId (Just (get #id team)) |> set #channelConfig (Aeson.object []) |> updateRecord
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJob <- expectOne =<< query @MattermostJob |> filterWhere (#alertId, alertId) |> fetch
            perform notifyJob
            posts <- mockPosts
            (root, _reply) <- expectRootAndReply posts
            postMessage root `shouldBe` "[FIRING] integration test alert"

    it "ack click resolves the actor by display name, else the service account" do
        suffix <- tshow <$> nextRandom
        withMattermostEnv do
            mockReset
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn ("mm-itest-click-" <> suffix) fp Firing){severity = "warning"})
            named <-
                newRecord @User
                    |> set #email ("mm-clicker-" <> suffix <> "@dev")
                    |> set #displayName ("mm-clicker-" <> suffix)
                    |> set #passwordHash "unused"
                    |> createRecord
            result <- ackFromMattermost alertId named.displayName
            result `shouldBe` Right ("Acked by " <> named.displayName)
            acked <- fetch alertId
            acked.status `shouldBe` "ack"
            acked.acknowledgedBy `shouldBe` Just named.id

            fp2 <- freshFingerprint
            Just alertId2 <- ingest source ((testEventIn ("mm-itest-click-" <> suffix) fp2 Firing){severity = "warning"})
            result2 <- ackFromMattermost alertId2 "no-such-mattermost-user"
            result2 `shouldBe` Right "Acked by Mattermost"
            acked2 <- fetch alertId2
            acked2.status `shouldBe` "ack"
            serviceAccount <-
                query @User
                    |> filterWhere (#email, "mattermost@localhost" :: Text)
                    |> fetchOne
            acked2.acknowledgedBy `shouldBe` Just serviceAccount.id

    it "fireTestAlert drives a [TEST] alert through the notification + escalation pipeline" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-fire-env-" <> suffix
        withMattermostEnv do
            mockReset
            created <- integrationSource "alertmanager" ("itest-fire-" <> suffix) "" (Aeson.object [])
            source <- created |> set #env envName |> updateRecord
            rule <- mattermostRule ("mm-fire-rule-" <> suffix) ("alerts-fire-" <> suffix) envName
            user <- testUser
            policy <-
                newRecord @EscalationPolicy
                    |> set #name ("mm-fire-pol-" <> suffix)
                    |> set
                        #steps
                        ( Aeson.toJSON
                            [ Aeson.object
                                [ "after_seconds" Aeson..= (0 :: Int)
                                , "target_user_id" Aeson..= tshow (get #id user)
                                ]
                            ]
                        )
                    |> createRecord
            _ <- rule |> set #escalationPolicyId (Just (get #id policy)) |> updateRecord

            alertId <- fireTestAlert source
            exposeFor alertId
            alert <- fetch alertId
            alert.title `shouldSatisfy` ("[TEST]" `Text.isInfixOf`)
            alert.fingerprint `shouldSatisfy` ("test:" `Text.isPrefixOf`)
            alert.status `shouldBe` "firing"

            notifyJob <- expectOne =<< query @MattermostJob |> filterWhere (#alertId, alertId) |> fetch
            perform notifyJob
            _ <- expectRootAndReply =<< mockPosts
            trackers <- query @EscalationTracker |> filterWhere (#alertId, alertId) |> fetch
            length trackers `shouldBe` 1

    it "notificationChannels provision upserts a channel that rules reference by name" do
        suffix <- tshow <$> nextRandom
        let channelName = "mm-prov-" <> suffix
        withMattermostEnv do
            mockReset
            base <- mockUrl
            m7Apply
                ( Aeson.object
                    [ "notificationChannels"
                        Aeson..= Aeson.object
                            [ Key.fromText channelName
                                Aeson..= Aeson.object
                                    [ "type" Aeson..= ("mattermost" :: Text)
                                    , "baseUrl" Aeson..= base
                                    , "tokenEnv" Aeson..= ("MATTERMOST_TOKEN" :: Text)
                                    ]
                            ]
                    , "notificationRules"
                        Aeson..= Aeson.object
                            [ Key.fromText ("mm-prov-rule-" <> suffix)
                                Aeson..= Aeson.object
                                    [ "channel" Aeson..= channelName
                                    , "match" Aeson..= Aeson.object ["fields" Aeson..= Aeson.object ["env" Aeson..= ("mm-prov-env-" <> suffix)]]
                                    , "channelConfig" Aeson..= Aeson.object ["team" Aeson..= ("mock" :: Text), "channel" Aeson..= channelName]
                                    ]
                            ]
                    ]
                )
            channel <- query @NotificationChannel |> filterWhere (#name, channelName) |> fetchOne
            channel.type_ `shouldBe` "mattermost"
            channel.baseUrl `shouldBe` base
            channel.protected `shouldBe` True
            rule <- query @NotificationRule |> filterWhere (#name, "mm-prov-rule-" <> suffix) |> fetchOne
            rule.channel `shouldBe` channelName
            -- the provisioned channel delivers end to end
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn ("mm-prov-env-" <> suffix) fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJobs <- query @MattermostJob |> filterWhere (#alertId, alertId) |> fetch
            notifyJob <- expectOne notifyJobs
            perform notifyJob
            posts <- mockPosts
            _ <- expectRootAndReply posts
            pure ()

    it "provisioning a rule referencing a missing channel aborts with a clear error" do
        suffix <- tshow <$> nextRandom
        outcome <- try (m7Apply (Aeson.object ["notificationRules" Aeson..= Aeson.object [Key.fromText ("mm-bad-rule-" <> suffix) Aeson..= Aeson.object ["channel" Aeson..= ("no-such-channel-" <> suffix)]]]))
        case outcome of
            Left (ProvisionError err) -> err `shouldSatisfy` ("does not resolve to any notification channel" `Text.isInfixOf`)
            Right _ -> expectationFailure "expected ProvisionError"

mattermostRule :: (?modelContext :: ModelContext) => Text -> Text -> Text -> IO NotificationRule
mattermostRule name channel envName = do
    base <- mockUrl
    _ <-
        newRecord @NotificationChannel
            |> set #name channel
            |> set #type_ ("mattermost" :: Text)
            |> set #baseUrl base
            |> set #config (Aeson.object ["tokenEnv" Aeson..= ("MATTERMOST_TOKEN" :: Text)])
            |> set #enabled True
            |> createRecord
    newRecord @NotificationRule
        |> set #name name
        |> set #position 50
        |> set #enabled True
        |> set #match (Aeson.object ["fields" Aeson..= Aeson.object ["env" Aeson..= envName]])
        |> set #severityThreshold "info"
        |> set #channel channel
        |> set #channelConfig (Aeson.object ["team" Aeson..= ("mock" :: Text), "channel" Aeson..= channel])
        |> set #throttleSeconds 0
        |> createRecord

expectOne :: [a] -> IO a
expectOne [single] = pure single
expectOne others = expectationFailure (cs ("expected exactly one element, got " <> tshow (length others))) >> error "unreachable"

-- mattermostRule plus a "colors" mapping merged into the channel row config
-- (the severity/status → hex overrides that {{color}} resolves through).
mattermostRuleWithColors :: (?modelContext :: ModelContext) => Text -> Text -> Text -> Aeson.Value -> IO NotificationRule
mattermostRuleWithColors name channel envName colors = do
    rule <- mattermostRule name channel envName
    chan <- query @NotificationChannel |> filterWhere (#name, channel) |> fetchOne
    let merged = case chan.config of
            Aeson.Object object_ -> Aeson.Object (KeyMap.insert "colors" colors object_)
            other -> other
    void (chan |> set #config merged |> updateRecord)
    pure rule

-- The root-card sub-part template rows are global config shared by every MM
-- delivery, so the example deletes them before AND after (finally) — same
-- pattern as AgentSpec's mattermost_root template-tool test.
deleteMattermostSubTemplates :: (?modelContext :: ModelContext) => IO ()
deleteMattermostSubTemplates = void do
    sqlExecTyped
        [typedSql|
        DELETE FROM llm_prompt_templates
        WHERE name IN ('mattermost_status', 'mattermost_fields', 'mattermost_color')
    |]

attachmentFieldText :: Text -> Aeson.Value -> Text
attachmentFieldText key post = fromMaybe "" do
    att <- listToMaybe (postAttachments post)
    pure (fromMaybe "" (parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? Key.fromText key Aeson..!= "")) att))

attachmentPairs :: Aeson.Value -> [(Text, Text)]
attachmentPairs post = concatMap pairs (postAttachments post)
  where
    pairs att =
        fromMaybe [] do
            fields <- parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "fields" Aeson..!= [])) att
            pure
                [ (title, value)
                | field <- fields
                , Just (title, value) <-
                    [ parseMaybe
                        ( Aeson.withObject
                            "field"
                            (\o -> (,) <$> o Aeson..:? "title" Aeson..!= "" <*> o Aeson..:? "value" Aeson..!= "")
                        )
                        field
                    ]
                ]

expectRootAndReply :: [Aeson.Value] -> IO (Aeson.Value, Aeson.Value)
expectRootAndReply (root : reply : _) = pure (root, reply)
expectRootAndReply others = expectationFailure (cs ("expected root + reply posts, got " <> tshow (length others))) >> error "unreachable"

withMattermostEnv :: IO a -> IO a
withMattermostEnv action = do
    oldToken <- lookupEnv "MATTERMOST_TOKEN"
    oldBase <- lookupEnv "HALEMANS_BASE_URL"
    oldActionSecret <- lookupEnv "MATTERMOST_ACTION_SECRET"
    flip
        finally
        ( restoreEnv "MATTERMOST_TOKEN" oldToken
            >> restoreEnv "HALEMANS_BASE_URL" oldBase
            >> restoreEnv "MATTERMOST_ACTION_SECRET" oldActionSecret
        )
        do
            setEnv "MATTERMOST_TOKEN" "test-mattermost-token"
            setEnv "HALEMANS_BASE_URL" "http://127.0.0.1:28080"
            setEnv "MATTERMOST_ACTION_SECRET" "itest-action-secret"
            action

mockUrl :: IO Text
mockUrl = cs . fromMaybe "http://127.0.0.1:18088" <$> lookupEnv "MOCK_MATTERMOST_URL"

mockJson :: Text -> IO Aeson.Value
mockJson path = do
    base <- mockUrl
    output <- readProcess "curl" ["-sf", cs (base <> path)] ""
    maybe (error ("mock returned non-JSON for " <> cs path)) pure (Aeson.decode (cs output))

mockReset :: IO ()
mockReset = do
    base <- mockUrl
    _ <- readProcess "curl" ["-sf", "-X", "POST", cs (base <> "/debug/reset")] ""
    pure ()

mockPosts :: IO [Aeson.Value]
mockPosts = do
    payload <- mockJson "/debug/posts"
    pure (fromMaybe [] (parseMaybe (Aeson.withObject "posts" (\o -> o Aeson..: "posts")) payload))

postField :: Text -> Aeson.Value -> Text
postField key post =
    fromMaybe "" (parseMaybe (Aeson.withObject "post" (\o -> o Aeson..:? Key.fromText key Aeson..!= "")) post)

postMessage :: Aeson.Value -> Text
postMessage = postField "message"

postId :: Aeson.Value -> Text
postId = postField "id"

postRootId :: Aeson.Value -> Text
postRootId = postField "root_id"

postAttachments :: Aeson.Value -> [Aeson.Value]
postAttachments post = fromMaybe [] do
    props <- parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "props")) post
    pure (fromMaybe [] (parseMaybe (Aeson.withObject "props" (\o -> o Aeson..:? "attachments" Aeson..!= [])) props))

postActions :: Aeson.Value -> [Aeson.Value]
postActions post = concatMap (fromMaybe [] . parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "actions" Aeson..!= []))) (postAttachments post)

actionField :: Text -> Aeson.Value -> Maybe Text
actionField key action = parseMaybe (Aeson.withObject "action" (\o -> o Aeson..:? Key.fromText key Aeson..!= "")) action

actionNames :: Aeson.Value -> [Text]
actionNames post = [name | Just name <- map (actionField "name") (postActions post)]

actionUrlFor :: Text -> Aeson.Value -> Text
actionUrlFor name post =
    fromMaybe "" do
        action <- find (\a -> actionField "name" a == Just name) (postActions post)
        integration <- parseMaybe (Aeson.withObject "action" (\o -> o Aeson..: "integration")) action
        parseMaybe (Aeson.withObject "integration" (\o -> o Aeson..: "url")) integration
