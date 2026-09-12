module Test.LiveSpec where

import Application.Service.Live (Scope (..), isResetFrame, liveConnectionCount, parseScope, registry)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import IHP.Prelude
import Test.Hspec

-- Registry/scope protocol of the websocket broadcaster (milestone 12 §7).
spec :: Spec
spec = describe "Application.Service.Live" do
    describe "parseScope" do
        it "parses a dashboard scope frame" do
            parseScope "{\"type\":\"dashboard\"}" `shouldBe` Just ScopeDashboard
        it "parses an env scope frame with default filters" do
            parseScope "{\"type\":\"env\",\"name\":\"dev\"}" `shouldSatisfy` \case
                Just (ScopeEnv "dev" _) -> True
                _ -> False
        it "parses an alert scope frame" do
            let uuid = "0c0ee9a4-76b5-4c10-9d51-9f5b6e8c8f01" :: Text
            parseScope (cs ("{\"type\":\"alert\",\"id\":\"" <> uuid <> "\"}"))
                `shouldBe` Just (ScopeAlert (fromJust (UUID.fromText uuid)))
        it "parses a group scope frame" do
            let uuid = "0c0ee9a4-76b5-4c10-9d51-9f5b6e8c8f02" :: Text
            parseScope (cs ("{\"type\":\"group\",\"id\":\"" <> uuid <> "\"}"))
                `shouldBe` Just (ScopeGroup (fromJust (UUID.fromText uuid)))
        it "parses a user dashboard scope frame" do
            let uuid = "0c0ee9a4-76b5-4c10-9d51-9f5b6e8c8f03" :: Text
            parseScope (cs ("{\"type\":\"dash\",\"id\":\"" <> uuid <> "\"}"))
                `shouldBe` Just (ScopeUserDashboard (fromJust (UUID.fromText uuid)))
        it "rejects unknown scope types and malformed frames" do
            parseScope "{\"type\":\"nope\"}" `shouldBe` Nothing
            parseScope "not json" `shouldBe` Nothing

    describe "isResetFrame" do
        it "recognizes the turbolinks reset frame" do
            isResetFrame "{\"type\":\"reset\"}" `shouldBe` True
        it "ignores subscribe frames and garbage" do
            isResetFrame "{\"type\":\"env\",\"name\":\"dev\"}" `shouldBe` False
            isResetFrame "not json" `shouldBe` False

    describe "connection registry" do
        it "tracks registrations and unregistrations in liveConnectionCount" do
            before <- liveConnectionCount
            scopeRef <- newIORef []
            let send (_ :: Text) = pure ()
            let connId = fromJust (UUID.fromText "0c0ee9a4-76b5-4c10-9d51-9f5b6e8c8f04")
            modifyIORef' registry ((connId, scopeRef, send) :)
            during <- liveConnectionCount
            modifyIORef' registry (filter (\(cid, _, _) -> cid /= connId))
            after <- liveConnectionCount
            during `shouldBe` before + 1
            after `shouldBe` before
        it "reset clears a connection's scopes, subscribe appends" do
            scopeRef <- newIORef [ScopeDashboard]
            -- subscribe frame appends
            case parseScope "{\"type\":\"env\",\"name\":\"dev\"}" of
                Just scope -> modifyIORef' scopeRef (scope :)
                Nothing -> expectationFailure "env frame did not parse"
            scopes <- readIORef scopeRef
            length scopes `shouldBe` 2
            -- reset frame clears (mirrors liveBroadcastLoop)
            when (isResetFrame "{\"type\":\"reset\"}") (writeIORef scopeRef [])
            readIORef scopeRef `shouldReturn` ([] :: [Scope])
