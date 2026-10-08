module Test.MattermostSpec where

import Application.Service.Mattermost (mattermostTarget, mattermostUsernameFromSettings)
import Application.Service.Mattermost.Banner (BannerCounts (..), Trend (..), bannerBarColor, bannerSeverities, countsJson, parseCounts, renderBannerText, trendOf)
import Application.Service.Mattermost.Render
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Generated.Types
import IHP.ModelSupport (newRecord, textToId)
import IHP.Prelude
import Test.Hspec
import Web.Controller.NotificationChannels (channelConfigJson)
import Web.View.NotificationChannels.Form (knownColorKeys)

mkAlert :: Text -> Text -> Alert
mkAlert status severity =
    newRecord @Alert
        |> set #title "CPU load above 80%"
        |> set #fingerprint "mm-spec-fp"
        |> set #status status
        |> set #severity severity
        |> set #occurrences 3
        |> set #env (Just "production")

renderContext :: MattermostRenderContext
renderContext =
    MattermostRenderContext
        { mrcRuleName = "cpu-rules"
        , mrcAckedBy = Just "sre-1"
        , mrcAckedAt = Nothing
        , mrcClosedBy = Nothing
        , mrcActionUrl = Just "http://halemans.example/hooks/mattermost/actions/secret"
        , mrcAckUrl = Just "http://halemans.example/alerts/abc/ack-link?token=tok"
        , mrcAlertUrl = "http://halemans.example/alerts/abc"
        , mrcAlertId = "abc"
        }

attachment :: Aeson.Value -> Aeson.Value
attachment props =
    fromMaybe (error "no attachments") do
        attachments <- parseMaybe (Aeson.withObject "props" (\o -> o Aeson..: "attachments")) props
        case attachments of
            (first : _) -> Just (first :: Aeson.Value)
            [] -> Nothing

attachmentText :: Aeson.Value -> Text
attachmentText props =
    fromMaybe "" (parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "text" Aeson..!= "")) (attachment props))

attachmentColor :: Aeson.Value -> Text
attachmentColor props =
    fromMaybe "" (parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "color" Aeson..!= "")) (attachment props))

fieldTitlesValues :: [Aeson.Value] -> [(Text, Text)]
fieldTitlesValues fields =
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

actionNames :: Aeson.Value -> [Text]
actionNames props =
    fromMaybe [] do
        actions <- parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "actions" Aeson..!= [])) (attachment props)
        pure [name | Just name <- map actionName (actions :: [Aeson.Value])]
  where
    actionName :: Aeson.Value -> Maybe Text
    actionName = parseMaybe (Aeson.withObject "action" (\o -> o Aeson..:? "name" Aeson..!= ""))

spec :: Spec
spec = describe "Application.Service.Mattermost.Render" do
    it "labels the root message with the uppercase state" do
        renderRootMessage Nothing [] renderContext (mkAlert "firing" "critical") `shouldBe` "[FIRING] CPU load above 80%"
        renderRootMessage Nothing [] renderContext (mkAlert "ack" "critical") `shouldBe` "[ACKED] CPU load above 80%"
        renderRootMessage Nothing [] renderContext (mkAlert "resolved" "warning") `shouldBe` "[RESOLVED] CPU load above 80%"

    it "renders the built-in fallback through the same slot path as DB templates" do
        let alert = mkAlert "firing" "critical"
        renderRootMessage (Just defaultRootTemplateBody) [] renderContext alert
            `shouldBe` renderRootMessage Nothing [] renderContext alert

    it "substitutes a custom root template" do
        let alert = mkAlert "firing" "critical"
        renderRootMessage (Just "{{alert.severity}} ({{rule}}): {{alert.title}}") [] renderContext alert
            `shouldBe` "critical (cpu-rules): CPU load above 80%"

    it "leaves unknown slots untouched" do
        let alert = mkAlert "firing" "critical"
        renderRootMessage (Just "[{{alert.state}}] {{alert.title}} {{unknown.slot}}") [] renderContext alert
            `shouldBe` "[FIRING] CPU load above 80% {{unknown.slot}}"

    it "drops blank lines left by empty slots in the details template" do
        let alert = mkAlert "firing" "critical"
            body = renderDetailsMessage (Just "**{{alert.title}}**\n{{alert.description}}\n\nSeverity: {{alert.severity}}") [] renderContext alert
        body `shouldBe` "**CPU load above 80%**\nSeverity: critical"

    it "colors by severity while firing" do
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "critical")) `shouldBe` "#E5484D"
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "warning")) `shouldBe` "#F7B500"
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "info")) `shouldBe` "#4C8DFF"

    it "colors terminal states gray regardless of severity" do
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "resolved" "critical")) `shouldBe` "#98A2AD"
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "closed" "critical")) `shouldBe` "#98A2AD"
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "stalled" "high")) `shouldBe` "#98A2AD"

    it "shows the ack actor on acked alerts" do
        attachmentText (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1"

    it "shows the ack timestamp alongside the actor" do
        let ackedContext = renderContext{mrcAckedAt = Just (UTCTime (fromGregorian 2026 10 1) (18 * 3600 + 30 * 60))}
        attachmentText (renderRootProps Nothing Nothing Nothing Nothing [] ackedContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1 at 2026-10-01 18:30 UTC"

    it "offers the Ack action only on firing alerts with an action URL" do
        actionNames (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high")) `shouldBe` ["Ack"]
        actionNames (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` []
        actionNames (renderRootProps Nothing Nothing Nothing Nothing [] renderContext{mrcActionUrl = Nothing} (mkAlert "firing" "high")) `shouldBe` []

    it "renders the one-time markdown Ack link on firing alerts only" do
        attachmentText (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high"))
            `shouldBe` "Firing · 3 occurrence(s) · [Ack](http://halemans.example/alerts/abc/ack-link?token=tok)"
        attachmentText (renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1"
        attachmentText (renderRootProps Nothing Nothing Nothing Nothing [] renderContext{mrcAckUrl = Nothing} (mkAlert "firing" "high"))
            `shouldBe` "Firing · 3 occurrence(s)"

    it "carries the alert id in the Ack action context" do
        let props = renderRootProps Nothing Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high")
            action = fromMaybe (error "no ack action") do
                actions <- parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "actions" Aeson..!= [])) (attachment props)
                case [a | a <- actions, actionId a == Just "ack"] of
                    (first : _) -> Just (first :: Aeson.Value)
                    [] -> Nothing
            actionId a = case parseMaybe (Aeson.withObject "action" (\o -> (o Aeson..:? "id") :: Parser (Maybe Text))) a of
                Just (Just id_) -> Just id_
                _ -> Nothing
            integration = fromMaybe (error "no integration") (parseMaybe (Aeson.withObject "action" (\o -> o Aeson..: "integration")) action)
            contextValue = fromMaybe (error "no context") (parseMaybe (Aeson.withObject "integration" (\o -> o Aeson..: "context")) (integration :: Aeson.Value))
        (parseMaybe (Aeson.withObject "context" (\o -> o Aeson..: "alertId")) contextValue :: Maybe Text) `shouldBe` Just "abc"

    it "renders details with description and the deep link" do
        let alert = (mkAlert "firing" "critical") |> set #description "load avg 15m above threshold"
            body = renderDetailsMessage Nothing [] renderContext alert
        body `shouldSatisfy` (Text.isInfixOf "load avg 15m above threshold")
        body `shouldSatisfy` (Text.isInfixOf "[Open in Halemans](http://halemans.example/alerts/abc)")

    it "renders no header line for an empty mattermost_root body" do
        renderRootMessage (Just "") [] renderContext (mkAlert "firing" "critical") `shouldBe` ""

    it "renders the admin card preview from the sample alert" do
        let preview = previewCard Nothing Nothing Nothing Nothing Nothing
        preview.mcpHeader `shouldBe` "[FIRING] Sample: CPU load above 80%"
        preview.mcpStatus `shouldSatisfy` (Text.isPrefixOf "Firing · 3 occurrence(s)")
        preview.mcpFields
            `shouldBe` [ ("Environment", "production")
                       , ("Host", "db-pgsql01")
                       , ("Service", "postgresql")
                       , ("Severity", "critical")
                       , ("Rule", "sample-rule")
                       ]
        preview.mcpColor `shouldBe` "#E5484D"
        preview.mcpAckAction `shouldBe` True

    it "preview reflects a custom header and suppressed fields" do
        let preview = previewCard (Just "") (Just "{{alert.env}}/{{alert.host}}") (Just "|") (Just "#123456") Nothing
        preview.mcpHeader `shouldBe` ""
        preview.mcpStatus `shouldBe` "production/db-pgsql01"
        preview.mcpFields `shouldBe` []
        preview.mcpColor `shouldBe` "#123456"

    it "renders a custom status template with the shared slots" do
        renderStatusMessage (Just "{{rule}}: {{alert.occurrences}}x {{alert.state}}") [] renderContext (mkAlert "firing" "high")
            `shouldBe` "cpu-rules: 3x FIRING"

    it "renders the built-in status template exactly like the old hardcoded lines" do
        renderStatusMessage Nothing [] renderContext (mkAlert "firing" "high")
            `shouldBe` "Firing · 3 occurrence(s) · [Ack](http://halemans.example/alerts/abc/ack-link?token=tok)"
        renderStatusMessage Nothing [] renderContext (mkAlert "ack" "high") `shouldBe` "Acked by sre-1"
        renderStatusMessage Nothing [] renderContext (mkAlert "resolved" "high") `shouldBe` "Resolved by the source"
        renderStatusMessage Nothing [] renderContext (mkAlert "stalled" "high") `shouldBe` "Stalled: no source updates"
        renderStatusMessage Nothing [] renderContext (mkAlert "closed" "high") `shouldBe` "Closed"

    it "renders the fields grid from Title|value template lines and drops empty values" do
        let fields = renderFields (Just "Rule|{{rule}}\nGone|{{closed_by}}\nBare") [] renderContext (mkAlert "firing" "high")
        fieldTitlesValues fields `shouldBe` [("Rule", "cpu-rules")]

    it "falls back to the built-in fields grid" do
        fieldTitlesValues (renderFields Nothing [] renderContext (mkAlert "firing" "high"))
            `shouldBe` [ ("Environment", "production")
                       , ("Host", "-")
                       , ("Service", "-")
                       , ("Severity", "high")
                       , ("Rule", "cpu-rules")
                       ]

    it "resolves the color bar from the config mapping with status winning over severity" do
        renderColor Nothing [("critical", "#123456")] renderContext (mkAlert "firing" "critical") `shouldBe` "#123456"
        renderColor Nothing [("resolved", "#AAAAAA"), ("critical", "#123456")] renderContext (mkAlert "resolved" "critical") `shouldBe` "#AAAAAA"
        renderColor Nothing [] renderContext (mkAlert "firing" "info") `shouldBe` "#4C8DFF"

    it "accepts a literal color template and falls back when it renders empty" do
        renderColor (Just "#FF00FF") [] renderContext (mkAlert "firing" "critical") `shouldBe` "#FF00FF"
        renderColor (Just "{{unknown_slot}}") [] renderContext (mkAlert "firing" "critical") `shouldBe` "#E5484D"

    it "parses color overrides from the channel config JSON" do
        colorMapFromJson (Aeson.object ["colors" Aeson..= Aeson.object ["warning" Aeson..= Aeson.String "#010203"]])
            `shouldBe` [("warning", "#010203")]
        colorMapFromJson (Aeson.object []) `shouldBe` []
        colorMapFromJson (Aeson.object ["colors" Aeson..= Aeson.String "nope"]) `shouldBe` []

    it "passes channel color overrides through to the attachment color" do
        let overrides = colorMapFromJson (Aeson.object ["colors" Aeson..= Aeson.object ["high" Aeson..= Aeson.String "#0A0B0C"]])
        attachmentColor (renderRootProps Nothing Nothing Nothing Nothing overrides renderContext (mkAlert "firing" "high"))
            `shouldBe` "#0A0B0C"

    it "maps template names to their built-in default bodies" do
        defaultTemplateBodyFor "mattermost_root" `shouldBe` Just "[{{alert.state}}] {{alert.title}}"
        defaultTemplateBodyFor "mattermost_color" `shouldBe` Just "{{color}}"
        defaultTemplateBodyFor "mattermost_status" `shouldSatisfy` maybe False (Text.isInfixOf "{{line_firing}}")
        defaultTemplateBodyFor "alert_enrichment" `shouldBe` Nothing

    it "reads the ackAction flag from the channel config JSON" do
        ackActionEnabledFromJson (Aeson.object []) `shouldBe` True
        ackActionEnabledFromJson (Aeson.object ["ackAction" Aeson..= Aeson.Bool False]) `shouldBe` False
        ackActionEnabledFromJson (Aeson.object ["ackAction" Aeson..= Aeson.Bool True]) `shouldBe` True
        ackActionEnabledFromJson (Aeson.object ["ackAction" Aeson..= Aeson.String "no"]) `shouldBe` True

    it "reads the deleteOnClose flag from the channel config JSON" do
        deleteOnCloseEnabledFromJson (Aeson.object []) `shouldBe` False
        deleteOnCloseEnabledFromJson (Aeson.object ["deleteOnClose" Aeson..= Aeson.Bool True]) `shouldBe` True
        deleteOnCloseEnabledFromJson (Aeson.object ["deleteOnClose" Aeson..= Aeson.Bool False]) `shouldBe` False
        deleteOnCloseEnabledFromJson (Aeson.object ["deleteOnClose" Aeson..= Aeson.String "yes"]) `shouldBe` False

    it "renders extra attachment properties from key|value lines with a whitelist" do
        let props = renderAttachmentPropPairs (Just "footer|Halemans · {{alert.state}}\ntitle|{{alert.title}}\nthumb_url| https://example/t.png \nbogus|nope\nts|1730000000\nempty|") [] renderContext (mkAlert "firing" "critical")
        props
            `shouldBe` [ ("footer", "Halemans · FIRING")
                       , ("title", "CPU load above 80%")
                       , ("thumb_url", "https://example/t.png")
                       , ("ts", "1730000000")
                       ]
        renderAttachmentPropPairs Nothing [] renderContext (mkAlert "firing" "critical") `shouldBe` []
        renderAttachmentPropPairs (Just "") [] renderContext (mkAlert "firing" "critical") `shouldBe` []

    it "emits ts as a JSON number and other props as strings in the attachment" do
        let att = attachment (renderRootProps Nothing Nothing Nothing (Just "footer|f\nts|1730000000") [] renderContext (mkAlert "firing" "critical"))
            footer = parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..:? "footer" Aeson..!= "")) att
            ts = parseMaybe (Aeson.withObject "attachment" (\o -> o Aeson..: "ts")) att
        footer `shouldBe` Just ("f" :: Text)
        ts `shouldBe` Just (1730000000 :: Integer)

    it "syncs only state-changing event kinds" do
        syncsKind "ack" `shouldBe` True
        syncsKind "unack" `shouldBe` True
        syncsKind "repeated" `shouldBe` True
        syncsKind "resolved" `shouldBe` True
        syncsKind "closed" `shouldBe` True
        syncsKind "stalled" `shouldBe` True
        syncsKind "created" `shouldBe` False
        syncsKind "comment" `shouldBe` False
        syncsKind "writeback" `shouldBe` False
        syncsKind "external" `shouldBe` False

mkRule :: Aeson.Value -> NotificationRule
mkRule channelConfig =
    newRecord @NotificationRule
        |> set #name "cpu-rules"
        |> set #channelConfig channelConfig

teamDefaults :: Aeson.Value
teamDefaults = fromMaybe (error "bad test JSON") (Aeson.decode "{\"mattermost\":{\"team\":\"sre\",\"channel\":\"oncall\"}}")

usernameSpec :: Spec
usernameSpec = describe "Application.Service.Mattermost.mattermostUsernameFromSettings" do
    it "defaults to empty when unset" do
        mattermostUsernameFromSettings (Aeson.object []) `shouldBe` ""
    it "reads the stored username" do
        mattermostUsernameFromSettings (Aeson.object ["mattermostUsername" Aeson..= Aeson.String "john.doe"]) `shouldBe` "john.doe"

targetSpec :: Spec
targetSpec = describe "Application.Service.Mattermost.mattermostTarget" do
    it "uses the rule channelConfig when set" do
        let rule = mkRule (fromMaybe (error "bad test JSON") (Aeson.decode "{\"team\":\"cfg-team\",\"channel\":\"cfg-chan\"}"))
        mattermostTarget teamDefaults rule `shouldBe` Right ("cfg-team", "cfg-chan")

    it "falls back to the team defaults and the halemans team name" do
        mattermostTarget teamDefaults (mkRule (Aeson.object [])) `shouldBe` Right ("sre", "oncall")
        let noTeamName = fromMaybe (error "bad test JSON") (Aeson.decode "{\"mattermost\":{\"channel\":\"oncall\"}}")
        mattermostTarget noTeamName (mkRule (Aeson.object [])) `shouldBe` Right ("halemans", "oncall")

    it "errors when neither the rule nor the team names a channel" do
        let result = mattermostTarget (Aeson.object []) (mkRule (Aeson.object []))
        case result of
            Left err -> err `shouldSatisfy` Text.isInfixOf "has no channel"
            Right _ -> expectationFailure "expected Left"

bannerSpec :: Spec
bannerSpec = describe "Application.Service.Mattermost.Banner" do
    it "renders one labeled counter per severity with the trend arrow" do
        let items =
                [ ("critical", BannerCounts 5 3, TrendUp)
                , ("high", BannerCounts 2 1, TrendFlat)
                , ("warning", BannerCounts 0 0, TrendDown)
                , ("info", BannerCounts 1 1, TrendFlat)
                ]
        renderBannerText items
            `shouldBe` "🔴 crit 5 (3)🔺 · 🟠 high 2 (1)➖ · 🟡 warn 0 (0)🔻 · 🔵 info 1 (1)➖"

    it "collapses to the all-clear line when nothing is active" do
        let items = [(sev, BannerCounts 0 0, TrendFlat) | sev <- bannerSeverities]
        renderBannerText items `shouldBe` "✅ no active alerts"

    it "keeps the bar neutral while anything is active, green when clear" do
        bannerBarColor [("critical", BannerCounts 0 0), ("info", BannerCounts 1 0)]
            `shouldBe` "#98A2AD"
        bannerBarColor [(sev, BannerCounts 0 0) | sev <- bannerSeverities]
            `shouldBe` "#3FB950"

    it "compares totals for the trend direction" do
        trendOf 3 5 `shouldBe` TrendUp
        trendOf 5 3 `shouldBe` TrendDown
        trendOf 4 4 `shouldBe` TrendFlat

    it "round-trips the snapshot counts JSON" do
        let counts = [("critical", BannerCounts 5 3), ("warning", BannerCounts 0 0)]
        parseCounts (countsJson counts) `shouldBe` counts

channelFormSpec :: Spec
channelFormSpec = describe "Web.Controller.NotificationChannels.channelConfigJson" do
    let base = Aeson.object ["custom" Aeson..= Aeson.String "kept", "colors" Aeson..= Aeson.object ["night" Aeson..= Aeson.String "#000000", "critical" Aeson..= Aeson.String "#111111"]]
        configKey key value = parseMaybe (Aeson.withObject "config" (\o -> o Aeson..: key)) value :: Maybe Aeson.Value

    it "writes banner + trend minutes and preserves unknown config keys" do
        let result = channelConfigJson base "MATTERMOST_TOKEN" True False True (Just 60) [(key, "") | key <- knownColorKeys]
        configKey "banner" result `shouldBe` Just (Aeson.Bool True)
        configKey "bannerTrendMinutes" result `shouldBe` Just (Aeson.Number 60)
        configKey "custom" result `shouldBe` Just (Aeson.String "kept")
        configKey "tokenEnv" result `shouldBe` Just (Aeson.String "MATTERMOST_TOKEN")

    it "turning banner off writes banner:false and drops a stored trend window" do
        let withWindow = channelConfigJson base "MATTERMOST_TOKEN" True False True (Just 60) [(key, "") | key <- knownColorKeys]
            result = channelConfigJson withWindow "MATTERMOST_TOKEN" True False False Nothing [(key, "") | key <- knownColorKeys]
        configKey "banner" result `shouldBe` Just (Aeson.Bool False)
        configKey "bannerTrendMinutes" result `shouldBe` Nothing

    it "omits an explicit default trend window" do
        let result = channelConfigJson (Aeson.object []) "MATTERMOST_TOKEN" True False True (Just 30) [(key, "") | key <- knownColorKeys]
        configKey "bannerTrendMinutes" result `shouldBe` Nothing

    it "merges form colors over stored ones, dropping empties and keeping unknown keys" do
        let result = channelConfigJson base "MATTERMOST_TOKEN" True False False Nothing [("critical", "#E5484D"), ("high", ""), ("warning", "#F7B500")]
            colors = configKey "colors" result
        (parseMaybe (Aeson.withObject "colors" (\o -> o Aeson..: "critical")) =<< colors) `shouldBe` (Just "#E5484D" :: Maybe Text)
        (parseMaybe (Aeson.withObject "colors" (\o -> o Aeson..: "warning")) =<< colors) `shouldBe` (Just "#F7B500" :: Maybe Text)
        (parseMaybe (Aeson.withObject "colors" (\o -> o Aeson..: "night")) =<< colors) `shouldBe` (Just "#000000" :: Maybe Text)
        (parseMaybe (Aeson.withObject "colors" (\o -> o Aeson..:? "high" Aeson..!= "")) =<< colors) `shouldBe` Just ("" :: Text)

    it "drops the colors object entirely when every form input is empty and nothing unknown remains" do
        let plain = Aeson.object ["colors" Aeson..= Aeson.object ["critical" Aeson..= Aeson.String "#111111"]]
            result = channelConfigJson plain "MATTERMOST_TOKEN" True False False Nothing [(key, "") | key <- knownColorKeys]
        configKey "colors" result `shouldBe` Nothing
