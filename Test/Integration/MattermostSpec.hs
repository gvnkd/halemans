module Test.Integration.MattermostSpec (spec) where

import Application.Helper.Ingest (NormalizedEvent (..), SourceStatus (..), ingest)
import Application.Job.Mattermost (enqueueMissingCards)
import Application.Pipeline.Actions (ackAlert)
import Application.Service.Mattermost (PurgeMattermostMode (..), PurgeMattermostSummary (..), purgeMattermostPostsForChannel, purgeResolvedMattermostPosts)
import Application.Service.Mattermost.Actions (ackFromMattermost)
import qualified Application.Service.Mattermost.Api as MM.Api
import Application.Service.Mattermost.Banner (BannerOutcome (..), refreshChannelBanner)
import Application.Service.Mattermost.Render (ackActionEnabledFromJson, colorMapFromJson)
import Application.Service.Provision (ProvisionError (..))
import Application.Service.TestAlert (fireTestAlert)
import Control.Exception (IOException, finally, try)
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.List (nub)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import Generated.Types
import IHP.Fetch (fetch, fetchOne)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.Job.Types (Job (..))
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.TypedSql (sqlExecTyped, sqlQueryTyped, typedSql)
import System.Environment (lookupEnv, setEnv)
import System.Process (readProcess)
import Test.Hspec
import Test.Integration.Setup (exposeFor, freshFingerprint, groupingRule, integrationSource, m7Apply, notifiedEvents, restoreEnv, testEventIn, testSource, testUser)

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
                    |> filterWhere (#alertId, Just alertId)
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
                    |> filterWhere (#alertId, Just alertId)
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
                    rule <- mattermostRuleWithColors ruleName channelName envName (Aeson.object ["colors" Aeson..= Aeson.object ["warning" Aeson..= Aeson.String "#0A0B0C"]])
                    _ <- pure rule
                    source <- testSource
                    fp <- freshFingerprint
                    Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
                    exposeFor alertId
                    notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
                    perform notifyJob

                    posts <- mockPosts
                    (root, _) <- expectRootAndReply posts
                    attachmentFieldText "text" root `shouldBe` ("FIRING x1 via " <> ruleName)
                    attachmentFieldText "color" root `shouldBe` "#0A0B0C"
                    attachmentPairs root `shouldBe` [("Env", envName), ("Sev", "warning")]
                )
                `finally` deleteMattermostSubTemplates

    it "renders extra attachment props (footer/title/thumb_url) from mattermost_attachment" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-props-env-" <> suffix
        withMattermostEnv do
            ( do
                    mockReset
                    deleteMattermostSubTemplates
                    _ <- newRecord @LlmPromptTemplate |> set #name "mattermost_attachment" |> set #version 1 |> set #body "footer|Halemans {{alert.state}}\ntitle|{{alert.title}}\nthumb_url|https://example/t.png" |> set #active True |> createRecord
                    _ <- mattermostRule ("mm-props-rule-" <> suffix) ("mm-props-chan-" <> suffix) envName
                    source <- testSource
                    fp <- freshFingerprint
                    Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
                    exposeFor alertId
                    notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
                    perform notifyJob

                    posts <- mockPosts
                    (root, _) <- expectRootAndReply posts
                    attachmentFieldText "footer" root `shouldBe` "Halemans FIRING"
                    attachmentFieldText "title" root `shouldBe` "integration test alert"
                    attachmentFieldText "thumb_url" root `shouldBe` "https://example/t.png"
                )
                `finally` deleteMattermostSubTemplates

    it "channel config ackAction:false hides the Ack button but keeps the markdown link" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-noack-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-noack-rule-" <> suffix) ("mm-noack-chan-" <> suffix) envName (Aeson.object ["ackAction" Aeson..= Aeson.Bool False])
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob

            posts <- mockPosts
            (root, _) <- expectRootAndReply posts
            actionNames root `shouldBe` []
            attachmentFieldText "text" root `shouldSatisfy` (Text.isInfixOf "[Ack](")

    it "channel config deleteOnClose:true deletes the root post when the alert resolves" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-del-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-del-rule-" <> suffix) ("mm-del-chan-" <> suffix) envName (Aeson.object ["deleteOnClose" Aeson..= Aeson.Bool True])
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob
            posts <- mockPosts
            (root, _reply) <- expectRootAndReply posts

            -- an ack is NOT terminal: the sync still patches the card
            alert <- fetch alertId
            user <- testUser
            _ <- ackAlert user alert Nothing Nothing
            ackSyncJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "sync" :: Text) |> fetch)
            perform ackSyncJob
            postsAfterAck <- mockPosts
            (rootAfterAck, _) <- expectRootAndReply postsAfterAck
            postMessage rootAfterAck `shouldBe` "[ACKED] integration test alert"
            postId rootAfterAck `shouldBe` postId root

            -- a resolve IS terminal: the sync deletes the root post + row
            void (ingest source ((testEventIn envName fp Resolved){severity = "warning"}))
            syncJobs <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "sync" :: Text) |> fetch
            resolvedSyncJob <- expectOne [job | job <- syncJobs, job.eventKind == Just "resolved"]
            perform resolvedSyncJob
            postsAfterResolve <- mockPosts
            [postId p | p <- postsAfterResolve, postId p == postId root] `shouldBe` []
            remainingRows <- query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch
            remainingRows `shouldBe` []

    it "a refire after deleteOnClose removed the card re-delivers a fresh card" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-refire-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-refire-rule-" <> suffix) ("mm-refire-chan-" <> suffix) envName (Aeson.object ["deleteOnClose" Aeson..= Aeson.Bool True])
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob
            posts <- mockPosts
            (root, _reply) <- expectRootAndReply posts

            -- resolve: deleteOnClose deletes the root post + drops the row
            -- (the resolve dispatch also enqueues a notify job; it syncs
            -- through deliverNotify and performs the same delete)
            let notifyJobsFor = query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch
            before <- notifyJobsFor
            void (ingest source ((testEventIn envName fp Resolved){severity = "warning"}))
            mid <- notifyJobsFor
            forM_ [job | job <- mid, get #id job `notElem` map (get #id) before] perform
            postsAfterResolve <- mockPosts
            [postId p | p <- postsAfterResolve, postId p == postId root] `shouldBe` []
            query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch >>= shouldBe []

            -- refire: the row is gone, so a sync would no-op — the
            -- transition must enqueue a fresh notify instead
            void (ingest source ((testEventIn envName fp Firing){severity = "warning"}))
            after <- notifyJobsFor
            refireNotifyJob <- expectOne [job | job <- after, get #id job `notElem` map (get #id) mid]
            perform refireNotifyJob
            postsAfterRefire <- mockPosts
            -- /debug/posts is insertion order: the original details reply
            -- (orphaned when the root was deleted) comes before the fresh
            -- root — pick the root by predicate, not position
            freshRoot <- expectOne [p | p <- postsAfterRefire, postRootId p == ""]
            postId freshRoot `shouldNotBe` postId root
            postMessage freshRoot `shouldBe` "[FIRING] integration test alert"
            row <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch
            row.rootPostId `shouldBe` postId freshRoot

    it "a grouped member inside the throttle window still gets its own mattermost card" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-grp-card-env-" <> suffix
            channelName = "mm-grp-card-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- groupingRule ("mm-grp-card-grp-" <> suffix) "{env}/{host}"
            rule <- mattermostRule ("mm-grp-card-rule-" <> suffix) channelName envName
            -- the default 300s group throttle: without the mattermost
            -- bypass the second member's dispatch would be dropped (the
            -- banner counts it, the channel never gets its card)
            void (rule |> set #throttleSeconds 300 |> updateRecord)
            source <- testSource
            fp1 <- freshFingerprint
            fp2 <- freshFingerprint
            Just alertId1 <- ingest source ((testEventIn envName fp1 Firing){severity = "warning"})
            Just alertId2 <- ingest source ((testEventIn envName fp2 Firing){severity = "warning"})
            exposeFor alertId1
            exposeFor alertId2
            notified2 <- notifiedEvents alertId2
            length notified2 `shouldBe` 1
            notifyJobs <-
                query @MattermostJob
                    |> filterWhere (#kind, "notify" :: Text)
                    |> fetch
            let memberJobs = [job | job <- notifyJobs, job.alertId == Just alertId1 || job.alertId == Just alertId2]
            length memberJobs `shouldBe` 2
    -- the human-notification throttle is untouched: a push rule on
    -- the same group still notifies once (covered in PipelineSpec)

    it "redeliver backfill enqueues cards only for matched alerts without a posts row" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-redeliver-env-" <> suffix
            otherEnvName = "mm-redeliver-other-env-" <> suffix
            channelName = "mm-redeliver-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-redeliver-rule-" <> suffix) channelName envName
            source <- testSource
            -- cardless matched alert
            fp1 <- freshFingerprint
            Just cardlessId <- ingest source ((testEventIn envName fp1 Firing){severity = "warning"})
            exposeFor cardlessId
            -- cardless UNMATCHED alert (different env): must stay untouched
            fp2 <- freshFingerprint
            Just unmatchedId <- ingest source ((testEventIn otherEnvName fp2 Firing){severity = "warning"})
            exposeFor unmatchedId
            -- alert with an existing card: backfill must not duplicate it
            fp3 <- freshFingerprint
            Just postedId <- ingest source ((testEventIn envName fp3 Firing){severity = "warning"})
            exposeFor postedId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just postedId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob

            -- the expose-time dispatch already enqueued a notify for the
            -- cardless alert (the throttle bypass) — diff job ids to grab
            -- the backfill's own row
            let notifyJobsFor aid = query @MattermostJob |> filterWhere (#alertId, Just aid) |> filterWhere (#kind, "notify" :: Text) |> fetch
            before <- notifyJobsFor cardlessId
            enqueued <- enqueueMissingCards channelName
            enqueued `shouldBe` 1
            after <- notifyJobsFor cardlessId
            backfillJob <- expectOne [job | job <- after, get #id job `notElem` map (get #id) before]
            perform backfillJob
            card <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, cardlessId) |> fetch
            posts <- mockPosts
            cardPost <- expectOne [p | p <- posts, postId p == rootPostId card]
            postMessage cardPost `shouldBe` "[FIRING] integration test alert"
            query @MattermostPost |> filterWhere (#alertId, unmatchedId) |> fetch >>= shouldBe []

    it "admin purge walks the channel and deletes only resolved alerts' bot root posts" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-purge-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-purge-rule-" <> suffix) ("mm-purge-chan-" <> suffix) envName
            source <- testSource
            -- resolved WITHOUT deleteOnClose: the sync patches the card
            -- terminal-gray and keeps the row — the purge deletes it
            -- retroactively
            fp <- freshFingerprint
            Just resolvedAlertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor resolvedAlertId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just resolvedAlertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob
            posts <- mockPosts
            (root, _reply) <- expectRootAndReply posts
            void (ingest source ((testEventIn envName fp Resolved){severity = "warning"}))
            syncJobs <- query @MattermostJob |> filterWhere (#alertId, Just resolvedAlertId) |> filterWhere (#kind, "sync" :: Text) |> fetch
            resolvedSyncJob <- expectOne [job | job <- syncJobs, job.eventKind == Just "resolved"]
            perform resolvedSyncJob
            postsAfterResolve <- mockPosts
            (rootAfterResolve, _) <- expectRootAndReply postsAfterResolve
            postId rootAfterResolve `shouldBe` postId root

            -- a still-firing alert's post must survive the purge
            fp2 <- freshFingerprint
            Just firingAlertId <- ingest source ((testEventIn envName fp2 Firing){severity = "warning"})
            exposeFor firingAlertId
            notifyJob2 <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just firingAlertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob2
            posts2 <- mockPosts
            firingRoot <- expectOne [p | p <- posts2, postRootId p == "", postId p /= postId root]

            -- a target whose server is unreachable is counted + reported,
            -- not silently skipped
            _ <- brokenBaseUrlRule ("mm-purge-broken-rule-" <> suffix) ("mm-purge-broken-chan-" <> suffix)

            summary <- purgeResolvedMattermostPosts
            summary.pmsPurged `shouldSatisfy` (>= 1)
            summary.pmsFailed `shouldBe` 0
            summary.pmsUntracked `shouldBe` 0
            summary.pmsKeptActive `shouldSatisfy` (>= 1)
            summary.pmsTargetsFailed `shouldSatisfy` (>= 1)
            summary.pmsErrors `shouldSatisfy` any ("127.0.0.1" `Text.isInfixOf`)
            postsAfterPurge <- mockPosts
            [postId p | p <- postsAfterPurge, postId p == postId root] `shouldBe` []
            [postId p | p <- postsAfterPurge, postId p == postId firingRoot] `shouldNotBe` []
            resolvedRows <- query @MattermostPost |> filterWhere (#alertId, resolvedAlertId) |> fetch
            resolvedRows `shouldBe` []
            firingRows <- query @MattermostPost |> filterWhere (#alertId, firingAlertId) |> fetch
            length firingRows `shouldBe` 1

    it "per-channel purge walks only that channel's rules' targets" do
        suffix <- tshow <$> nextRandom
        let envA = "mm-pcha-env-" <> suffix
            envB = "mm-pchb-env-" <> suffix
            chanAName = "mm-pcha-chan-" <> suffix
            chanBName = "mm-pchb-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-pcha-rule-" <> suffix) chanAName envA
            _ <- mattermostRule ("mm-pchb-rule-" <> suffix) chanBName envB
            source <- testSource
            -- resolved alert posts on BOTH channels (default: no
            -- deleteOnClose, so the rows survive for the retroactive purge)
            (alertA, fpA) <- fireAndResolve source envA
            (alertB, _fpB) <- fireAndResolve source envB
            rowA <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, alertA) |> fetch
            rowB <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, alertB) |> fetch

            chanA <- query @NotificationChannel |> filterWhere (#name, chanAName) |> fetchOne
            summary <- purgeMattermostPostsForChannel PurgeResolvedPosts chanA
            summary.pmsPurged `shouldBe` 1
            summary.pmsFailed `shouldBe` 0
            summary.pmsUntracked `shouldBe` 0
            summary.pmsKeptActive `shouldBe` 0
            summary.pmsTargetsFailed `shouldBe` 0
            posts <- mockPosts
            [postId p | p <- posts, postId p == rowA.rootPostId] `shouldBe` []
            [postId p | p <- posts, postId p == rowB.rootPostId] `shouldNotBe` []

    it "unrelated purge deletes every non-firing root post, including untracked leftovers" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-unrel-env-" <> suffix
            channelName = "mm-unrel-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-unrel-rule-" <> suffix) channelName envName
            source <- testSource
            -- a FIRING alert post: kept
            fpFiring <- freshFingerprint
            Just firingAlertId <- ingest source ((testEventIn envName fpFiring Firing){severity = "warning"})
            exposeFor firingAlertId
            notifyFiring <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just firingAlertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyFiring
            firingRow <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, firingAlertId) |> fetch
            -- a RESOLVED alert post: deleted
            (resolvedAlertId, _fpResolved) <- fireAndResolve source envName
            resolvedRow <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, resolvedAlertId) |> fetch
            -- an UNTRACKED bot root post (no mattermost_posts row): deleted
            chan <- query @NotificationChannel |> filterWhere (#name, channelName) |> fetchOne
            config <- fromMaybe (error "expected a usable mattermost config") <$> MM.Api.configForChannel chan
            Right channelId <- MM.Api.resolveChannel config "mock" channelName
            Right untrackedId <- MM.Api.createPost config channelId "untracked leftover" Nothing (Aeson.object [])

            summary <- purgeMattermostPostsForChannel PurgeUnrelatedPosts chan
            summary.pmsPurged `shouldBe` 2
            summary.pmsFailed `shouldBe` 0
            summary.pmsUntracked `shouldBe` 0
            summary.pmsKeptActive `shouldBe` 1
            summary.pmsTargetsFailed `shouldBe` 0
            posts <- mockPosts
            [postId p | p <- posts, postId p == firingRow.rootPostId] `shouldNotBe` []
            [postId p | p <- posts, postId p == resolvedRow.rootPostId] `shouldBe` []
            [postId p | p <- posts, postId p == untrackedId] `shouldBe` []
            -- the mapped rows went with the posts; the firing row stays
            resolvedRows <- query @MattermostPost |> filterWhere (#alertId, resolvedAlertId) |> fetch
            resolvedRows `shouldBe` []
            firingRows <- query @MattermostPost |> filterWhere (#alertId, firingAlertId) |> fetch
            length firingRows `shouldBe` 1

    it "channelPosts follows the before-post-id cursor past 200 posts" do
        suffix <- tshow <$> nextRandom
        let channelName = "mm-pages-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-pages-rule-" <> suffix) channelName ("mm-pages-env-" <> suffix)
            chan <- query @NotificationChannel |> filterWhere (#name, channelName) |> fetchOne
            config <- fromMaybe (error "expected a usable mattermost config") <$> MM.Api.configForChannel chan
            Right channelId <- MM.Api.resolveChannel config "mock" channelName
            forM_ [(1 :: Int) .. 205] \_ ->
                void (MM.Api.createPost config channelId "bulk" Nothing (Aeson.object []))
            postsResult <- MM.Api.channelPosts config channelId
            fmap length postsResult `shouldBe` Right 205
            fmap (length . nub . map postId) postsResult `shouldBe` Right 205

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
            jobs <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> fetch
            notifyJob <- expectOne jobs
            perform notifyJob
            void (ingest source ((testEventIn envName fp Firing){severity = "warning"}))
            jobsAgain <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> fetch
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
            notifyJob <- expectOne =<< query @MattermostJob |> filterWhere (#alertId, Just alertId) |> fetch
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

            notifyJob <- expectOne =<< query @MattermostJob |> filterWhere (#alertId, Just alertId) |> fetch
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
                                    , "config"
                                        Aeson..= Aeson.object
                                            [ "ackAction" Aeson..= Aeson.Bool False
                                            , "colors" Aeson..= Aeson.object ["warning" Aeson..= ("#010203" :: Text)]
                                            ]
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
            ackActionEnabledFromJson channel.config `shouldBe` False
            colorMapFromJson channel.config `shouldBe` [("warning", "#010203")]
            -- re-provisioning with only the managed fields keeps hand-set extras
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
                    ]
                )
            channelAgain <- query @NotificationChannel |> filterWhere (#name, channelName) |> fetchOne
            ackActionEnabledFromJson channelAgain.config `shouldBe` False
            colorMapFromJson channelAgain.config `shouldBe` [("warning", "#010203")]
            rule <- query @NotificationRule |> filterWhere (#name, "mm-prov-rule-" <> suffix) |> fetchOne
            rule.channel `shouldBe` channelName
            -- the provisioned channel delivers end to end
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn ("mm-prov-env-" <> suffix) fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJobs <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> fetch
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

    it "recreates the channel card when the tracked root post was deleted in Mattermost" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-recreate-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-recreate-rule-" <> suffix) ("mm-recreate-chan-" <> suffix) envName
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
            perform notifyJob
            (root, _reply) <- expectRootAndReply =<< mockPosts
            let rootId = postId root
            -- someone deletes the card in Mattermost while the alert is active
            mockDeletePost rootId
            user <- testUser
            alert <- fetch alertId
            _ <- ackAlert user alert Nothing Nothing
            syncJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "sync" :: Text) |> fetch)
            perform syncJob
            -- a fresh root + details pair replaces the deleted one, row re-pointed
            -- (the orphaned original details reply stays in the channel by design)
            posts <- mockPosts
            root' <- expectOne [p | p <- posts, postRootId p == "", postId p /= rootId]
            reply' <- expectOne [p | p <- posts, postRootId p == postId root']
            postMessage root' `shouldBe` "[ACKED] integration test alert"
            postRootId reply' `shouldBe` postId root'
            row <- expectOne =<< query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch
            row.rootPostId `shouldBe` postId root'

    it "a notify job landing after a fast source resolve posts no orphan card" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-late-env-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRule ("mm-late-rule-" <> suffix) ("mm-late-chan-" <> suffix) envName
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            exposeFor alertId
            -- resolve BEFORE the queued notify job runs: the resolve's sync
            -- no-ops (no posts yet), then the notify must not post a fresh
            -- card for the terminal alert either
            void (ingest source ((testEventIn envName fp Resolved){severity = "warning"}))
            -- both the expose-time notify and the resolve-dispatch notify
            -- must skip posting (the alert is terminal by the time they run)
            notifyJobs <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch
            length notifyJobs `shouldBe` 2
            forM_ notifyJobs perform
            posts <- mockPosts
            posts `shouldBe` []
            rows <- query @MattermostPost |> filterWhere (#alertId, alertId) |> fetch
            rows `shouldBe` []

    it "refreshes the channel banner with rule-scoped counts and trend arrows" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-banner-env-" <> suffix
            channelName = "mm-banner-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <-
                mattermostRuleWithConfig
                    ("mm-banner-rule-" <> suffix)
                    channelName
                    envName
                    ( Aeson.object
                        [ "banner" Aeson..= Aeson.Bool True
                        , "bannerTrendMinutes" Aeson..= (1 :: Int)
                        ]
                    )
            source <- testSource
            fp1 <- freshFingerprint
            Just critAlertId <- ingest source ((testEventIn envName fp1 Firing){severity = "critical"})
            exposeFor critAlertId
            fp2 <- freshFingerprint
            Just warnAlertId <- ingest source ((testEventIn envName fp2 Firing){severity = "warning"})
            exposeFor warnAlertId
            user <- testUser
            warnAlert <- fetch warnAlertId
            _ <- ackAlert user warnAlert Nothing Nothing

            -- the alert-event fan-out enqueued exactly one pending banner job
            -- for this channel (debounced pile-up guard)
            pendingCounts <-
                sqlQueryTyped
                    [typedSql|
                        SELECT count(*)::int FROM mattermost_jobs
                        WHERE kind = 'banner' AND channel = ${channelName}
                          AND status::text = 'job_status_not_started'
                    |] ::
                    IO [Int]
            pendingCounts `shouldBe` [1]

            -- history two minutes back (outside the 1-minute trend window
            -- anchor: zero critical, four warning) drives the arrows
            past <- addUTCTime (-120) <$> getCurrentTime
            _ <-
                newRecord @AlertStatsSnapshot
                    |> set #channel channelName
                    |> set #counts (snapshotCounts [("critical", 0, 0), ("warning", 4, 0)])
                    |> set #createdAt past
                    |> createRecord

            refreshChannelBanner channelName `shouldReturn` Right BannerRefreshed

            banners <- mockBanners
            banner <- expectOne banners
            bannerText banner `shouldBe` "🔴 crit 1 (0)🔺 · 🟠 high 0 (0)➖ · 🟡 warn 1 (1)🔻 · 🔵 info 0 (0)➖"
            bannerColor banner `shouldBe` "#98A2AD"

            -- a snapshot of the CURRENT counts was written for the next trend
            snapshots <-
                query @AlertStatsSnapshot
                    |> filterWhere (#channel, channelName)
                    |> fetch
            let newest = maximum (map (.createdAt) snapshots)
                current = [s | s <- snapshots, s.createdAt == newest]
            snapshot <- expectOne current
            countsField "critical" snapshot.counts `shouldBe` (1, 0)
            countsField "warning" snapshot.counts `shouldBe` (1, 1)

    it "shows the all-clear banner when the channel's rules match no active alerts" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-banner-clear-env-" <> suffix
            channelName = "mm-banner-clear-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <-
                mattermostRuleWithConfig
                    ("mm-banner-clear-rule-" <> suffix)
                    channelName
                    envName
                    (Aeson.object ["banner" Aeson..= Aeson.Bool True])
            refreshChannelBanner channelName `shouldReturn` Right BannerRefreshed
            banner <- expectOne =<< mockBanners
            bannerText banner `shouldBe` "✅ no active alerts"
            bannerColor banner `shouldBe` "#3FB950"

    it "banner counts exclude blackout-suppressed (muted) alerts" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-banner-mute-env-" <> suffix
            channelName = "mm-banner-mute-chan-" <> suffix
            critTitle = "mm-banner-muted-crit " <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-banner-mute-rule-" <> suffix) channelName envName (Aeson.object ["banner" Aeson..= Aeson.Bool True])
            now <- getCurrentTime
            _ <-
                newRecord @Blackout
                    |> set #titleGlob (Just critTitle)
                    |> set #startsAt (addUTCTime (-60) now)
                    |> set #endsAt (addUTCTime 3600 now)
                    |> set #reason "itest banner mute"
                    |> createRecord
            source <- testSource
            fp1 <- freshFingerprint
            Just critAlertId <- ingest source ((testEventIn envName fp1 Firing){severity = "critical", title = critTitle})
            exposeFor critAlertId
            crit <- fetch critAlertId
            crit.suppressed `shouldBe` True
            fp2 <- freshFingerprint
            Just warnAlertId <- ingest source ((testEventIn envName fp2 Firing){severity = "warning"})
            exposeFor warnAlertId
            refreshChannelBanner channelName `shouldReturn` Right BannerRefreshed
            banner <- expectOne =<< mockBanners
            bannerText banner `shouldBe` "🔴 crit 0 (0)➖ · 🟠 high 0 (0)➖ · 🟡 warn 1 (0)➖ · 🔵 info 0 (0)➖"

    it "banner counts stalled alerts as active" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-banner-stall-env-" <> suffix
            channelName = "mm-banner-stall-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-banner-stall-rule-" <> suffix) channelName envName (Aeson.object ["banner" Aeson..= Aeson.Bool True])
            source <- testSource
            fp <- freshFingerprint
            Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
            alert <- fetch alertId
            _ <- alert |> set #status "stalled" |> updateRecord
            refreshChannelBanner channelName `shouldReturn` Right BannerRefreshed
            banner <- expectOne =<< mockBanners
            bannerText banner `shouldBe` "🔴 crit 0 (0)➖ · 🟠 high 0 (0)➖ · 🟡 warn 1 (0)➖ · 🔵 info 0 (0)➖"

    it "a denied banner PUT (403) soft-skips without a snapshot, and recovers when allowed" do
        suffix <- tshow <$> nextRandom
        let envName = "mm-banner-deny-env-" <> suffix
            channelName = "mm-banner-deny-chan-" <> suffix
        withMattermostEnv do
            mockReset
            _ <- mattermostRuleWithConfig ("mm-banner-deny-rule-" <> suffix) channelName envName (Aeson.object ["banner" Aeson..= Aeson.Bool True])
            -- the first resolved MM channel after a reset is always chan-1
            mockBannerDeny "chan-1"
            refreshChannelBanner channelName `shouldReturn` Right BannerDenied
            mockBanners `shouldReturn` []
            rows <- query @AlertStatsSnapshot |> filterWhere (#channel, channelName) |> fetch
            rows `shouldBe` []
            mockBannerAllow "chan-1"
            refreshChannelBanner channelName `shouldReturn` Right BannerRefreshed
            banner <- expectOne =<< mockBanners
            bannerText banner `shouldBe` "✅ no active alerts"

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

-- Fire an alert, deliver the MM card, then resolve it and run the resolve
-- sync (default channel config: the card is patched terminal-gray and the
-- mattermost_posts row SURVIVES for the retroactive purges).
fireAndResolve :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Source -> Text -> IO (Id Alert, Text)
fireAndResolve source envName = do
    fp <- freshFingerprint
    Just alertId <- ingest source ((testEventIn envName fp Firing){severity = "warning"})
    exposeFor alertId
    notifyJob <- expectOne =<< (query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "notify" :: Text) |> fetch)
    perform notifyJob
    void (ingest source ((testEventIn envName fp Resolved){severity = "warning"}))
    syncJobs <- query @MattermostJob |> filterWhere (#alertId, Just alertId) |> filterWhere (#kind, "sync" :: Text) |> fetch
    resolvedSyncJob <- expectOne [job | job <- syncJobs, job.eventKind == Just "resolved"]
    perform resolvedSyncJob
    pure (alertId, fp)

expectOne :: [a] -> IO a
expectOne [single] = pure single
expectOne others = expectationFailure (cs ("expected exactly one element, got " <> tshow (length others))) >> error "unreachable"

-- mattermostRule plus extra config keys merged into the channel row config
-- (e.g. "colors" mapping for {{color}}, "ackAction": false to hide the
-- interactive Ack button).
mattermostRuleWithConfig :: (?modelContext :: ModelContext) => Text -> Text -> Text -> Aeson.Value -> IO NotificationRule
mattermostRuleWithConfig name channel envName extra = do
    rule <- mattermostRule name channel envName
    chan <- query @NotificationChannel |> filterWhere (#name, channel) |> fetchOne
    let merged = case (chan.config, extra) of
            (Aeson.Object base, Aeson.Object additions) -> Aeson.Object (KeyMap.union additions base)
            (base, _) -> base
    void (chan |> set #config merged |> updateRecord)
    pure rule

mattermostRuleWithColors :: (?modelContext :: ModelContext) => Text -> Text -> Text -> Aeson.Value -> IO NotificationRule
mattermostRuleWithColors = mattermostRuleWithConfig

-- A mattermost rule whose channel row points at an unreachable server —
-- the admin purge must count it as a failed target (reason in pmsErrors)
-- instead of skipping it silently.
brokenBaseUrlRule :: (?modelContext :: ModelContext) => Text -> Text -> IO NotificationRule
brokenBaseUrlRule name channel = do
    _ <-
        newRecord @NotificationChannel
            |> set #name channel
            |> set #type_ ("mattermost" :: Text)
            |> set #baseUrl ("http://127.0.0.1:1" :: Text)
            |> set #config (Aeson.object ["tokenEnv" Aeson..= ("MATTERMOST_TOKEN" :: Text)])
            |> set #enabled True
            |> createRecord
    newRecord @NotificationRule
        |> set #name name
        |> set #position 50
        |> set #enabled True
        |> set #match (Aeson.object ["fields" Aeson..= Aeson.object ["env" Aeson..= ("mm-purge-broken-env" :: Text)]])
        |> set #severityThreshold "info"
        |> set #channel channel
        |> set #channelConfig (Aeson.object ["team" Aeson..= ("mock" :: Text), "channel" Aeson..= channel])
        |> set #throttleSeconds 0
        |> createRecord

-- The root-card sub-part template rows are global config shared by every MM
-- delivery, so the example deletes them before AND after (finally) — same
-- pattern as AgentSpec's mattermost_root template-tool test.
deleteMattermostSubTemplates :: (?modelContext :: ModelContext) => IO ()
deleteMattermostSubTemplates = void do
    sqlExecTyped
        [typedSql|
        DELETE FROM llm_prompt_templates
        WHERE name IN ('mattermost_status', 'mattermost_fields', 'mattermost_color', 'mattermost_attachment')
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

mockBanners :: IO [Aeson.Value]
mockBanners = do
    payload <- mockJson "/debug/banners"
    pure (fromMaybe [] (parseMaybe (Aeson.withObject "banners" (\o -> o Aeson..: "banners")) payload))

mockBannerDeny :: Text -> IO ()
mockBannerDeny channelId = do
    base <- mockUrl
    _ <- readProcess "curl" ["-sf", "-X", "POST", cs (base <> "/debug/banner-deny"), "-H", "Content-Type: application/json", "-d", cs (Aeson.encode (Aeson.object ["channel_id" Aeson..= channelId]))] ""
    pure ()

mockBannerAllow :: Text -> IO ()
mockBannerAllow channelId = do
    base <- mockUrl
    _ <- readProcess "curl" ["-sf", "-X", "POST", cs (base <> "/debug/banner-allow"), "-H", "Content-Type: application/json", "-d", cs (Aeson.encode (Aeson.object ["channel_id" Aeson..= channelId]))] ""
    pure ()

mockDeletePost :: Text -> IO ()
mockDeletePost postId = do
    base <- mockUrl
    outcome <- try (readProcess "curl" ["-sf", "-X", "DELETE", cs (base <> "/api/v4/posts/" <> postId), "-H", "Authorization: Bearer test-mattermost-token"] "") :: IO (Either IOException String)
    case outcome of
        Right _ -> pure ()
        Left exception -> do
            listing <- mockJson "/debug/posts"
            let ids :: [Text]
                ids = [idText | Just idText <- map (parseMaybe (Aeson.withObject "post" (\o -> o Aeson..: "id"))) (fromMaybe [] (parseMaybe (Aeson.withObject "posts" (\o -> o Aeson..: "posts")) listing))]
            error (show exception <> " — mock posts now: " <> show ids)

bannerField :: Text -> Aeson.Value -> Text
bannerField key banner =
    fromMaybe "" (parseMaybe (Aeson.withObject "banner" (\o -> o Aeson..:? Key.fromText key Aeson..!= "")) banner)

bannerText :: Aeson.Value -> Text
bannerText = bannerField "text"

bannerColor :: Aeson.Value -> Text
bannerColor = bannerField "color"

snapshotCounts :: [(Text, Int, Int)] -> Aeson.Value
snapshotCounts entries =
    Aeson.object
        [ Key.fromText sev
            Aeson..= Aeson.object
                [ "total" Aeson..= total
                , "acked" Aeson..= acked
                ]
        | (sev, total, acked) <- entries
        ]

countsField :: Text -> Aeson.Value -> (Int, Int)
countsField sev value = fromMaybe (0, 0) do
    object_ <- case value of
        Aeson.Object o -> Just o
        _ -> Nothing
    raw <- KeyMap.lookup (Key.fromText sev) object_
    parseMaybe
        ( Aeson.withObject "severity counts" \o ->
            (,) <$> o Aeson..: "total" <*> o Aeson..: "acked"
        )
        raw

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
