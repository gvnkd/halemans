module Test.MattermostSpec where

import Application.Service.Mattermost (mattermostTarget, mattermostUsernameFromSettings)
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
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "critical")) `shouldBe` "#E5484D"
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "warning")) `shouldBe` "#F7B500"
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "info")) `shouldBe` "#4C8DFF"

    it "colors terminal states gray regardless of severity" do
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "resolved" "critical")) `shouldBe` "#98A2AD"
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "closed" "critical")) `shouldBe` "#98A2AD"
        attachmentColor (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "stalled" "high")) `shouldBe` "#98A2AD"

    it "shows the ack actor on acked alerts" do
        attachmentText (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1"

    it "shows the ack timestamp alongside the actor" do
        let ackedContext = renderContext{mrcAckedAt = Just (UTCTime (fromGregorian 2026 10 1) (18 * 3600 + 30 * 60))}
        attachmentText (renderRootProps Nothing Nothing Nothing [] ackedContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1 at 2026-10-01 18:30 UTC"

    it "offers the Ack action only on firing alerts with an action URL" do
        actionNames (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high")) `shouldBe` ["Ack"]
        actionNames (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` []
        actionNames (renderRootProps Nothing Nothing Nothing [] renderContext{mrcActionUrl = Nothing} (mkAlert "firing" "high")) `shouldBe` []

    it "renders the one-time markdown Ack link on firing alerts only" do
        attachmentText (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high"))
            `shouldBe` "Firing · 3 occurrence(s) · [Ack](http://halemans.example/alerts/abc/ack-link?token=tok)"
        attachmentText (renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "ack" "high")) `shouldBe` "Acked by sre-1"
        attachmentText (renderRootProps Nothing Nothing Nothing [] renderContext{mrcAckUrl = Nothing} (mkAlert "firing" "high"))
            `shouldBe` "Firing · 3 occurrence(s)"

    it "carries the alert id in the Ack action context" do
        let props = renderRootProps Nothing Nothing Nothing [] renderContext (mkAlert "firing" "high")
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
        attachmentColor (renderRootProps Nothing Nothing Nothing overrides renderContext (mkAlert "firing" "high"))
            `shouldBe` "#0A0B0C"

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
