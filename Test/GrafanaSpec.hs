module Test.GrafanaSpec where

import qualified Application.Connector.Grafana as Grafana
import Application.Service.Http (HttpStatusError (..))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight, isLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import IHP.Prelude
import Network.HTTP.Types (status200, status500)
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

-- Streaming fold over the alertmanager listing: the payload is served by a
-- local warp server (several MB, far exceeding one socket read), so the
-- fold is exercised across many refills. The regression this guards: the
-- old buffering alertsGet decoded the whole listing as one aeson value and
-- OOMed the worker on sources with huge alert counts. The mock dispatches
-- on the Bearer token because alertsFold fixes the URL path itself.
spec :: Spec
spec = describe "Application.Connector.Grafana.alertsFold" do
    around withServer do
        it "folds a multi-megabyte listing element by element" \(baseUrl, _) -> do
            firstRef <- newIORef Nothing :: IO (IORef (Maybe Text))
            result <-
                Grafana.alertsFold
                    (cs baseUrl)
                    "token"
                    ( \count alert -> do
                        modifyIORef' firstRef \case
                            Nothing -> Just alert.amFingerprint
                            just -> just
                        pure (count + (1 :: Int))
                    )
                    0
            result `shouldBe` Right 20000
            first <- readIORef firstRef
            first `shouldBe` Just "fp-1"

        it "decodes alert fields as they stream in" \(baseUrl, _) -> do
            result <-
                Grafana.alertsFold
                    (cs baseUrl)
                    "token"
                    (\acc alert -> pure (acc ++ [(alert.amFingerprint, alert.amGeneratorUrl)]))
                    []
            let alerts = fromRight [] result
            length alerts `shouldBe` 20000
            take 2 alerts
                `shouldBe` [ ("fp-1", Just "http://grafana/1")
                           , ("fp-2", Just "http://grafana/2")
                           ]

        it "folds an empty listing" \(baseUrl, _) -> do
            result <-
                Grafana.alertsFold
                    (cs baseUrl)
                    "empty"
                    (\count _ -> pure (count + (1 :: Int)))
                    (0 :: Int)
            result `shouldBe` Right 0

        it "returns Left on a malformed listing" \(baseUrl, _) -> do
            result <-
                Grafana.alertsFold
                    (cs baseUrl)
                    "bad"
                    (\count _ -> pure (count + (1 :: Int)))
                    (0 :: Int)
            result `shouldSatisfy` isLeft

        it "returns Left when an element misses the fingerprint" \(baseUrl, _) -> do
            result <-
                Grafana.alertsFold
                    (cs baseUrl)
                    "nofp"
                    (\count _ -> pure (count + (1 :: Int)))
                    (0 :: Int)
            result `shouldSatisfy` isLeft

        it "throws HttpStatusError on a non-2xx response" \(baseUrl, _) -> do
            let foldIt =
                    Grafana.alertsFold
                        (cs baseUrl)
                        "boom"
                        (\count _ -> pure (count + (1 :: Int)))
                        (0 :: Int)
            foldIt
                `shouldThrow` \case
                    HttpStatusError _ code -> code == 500

withServer :: ((String, IORef ()) -> IO ()) -> IO ()
withServer action = do
    port <- freePort
    seen <- newIORef ()
    ready <- newEmptyMVar
    let settings = Warp.setPort port (Warp.setBeforeMainLoop (putMVar ready ()) Warp.defaultSettings)
    _ <- forkIO (Warp.runSettings settings serverApp)
    takeMVar ready
    action ("http://localhost:" ++ cs (show port), seen)

serverApp :: Wai.Application
serverApp req respond = respond $ case lookup "Authorization" (Wai.requestHeaders req) of
    Just "Bearer empty" -> Wai.responseLBS status200 [("Content-Type", "application/json")] "[]"
    Just "Bearer bad" -> Wai.responseLBS status200 [("Content-Type", "application/json")] "{\"alerts\": ["
    Just "Bearer nofp" -> Wai.responseLBS status200 [("Content-Type", "application/json")] "[{\"labels\": {}}]"
    Just "Bearer boom" -> Wai.responseLBS status500 [] "boom"
    _ -> Wai.responseLBS status200 [("Content-Type", "application/json")] (bigPayload 20000)

bigPayload :: Int -> LBS.ByteString
bigPayload n = Aeson.encode [alert i | i <- [1 .. n]]
  where
    alert i =
        Aeson.object
            [ "fingerprint" Aeson..= ("fp-" <> tshow i)
            , "status" Aeson..= ("firing" :: Text)
            , "labels"
                Aeson..= Aeson.object
                    [ "alertname" Aeson..= ("rule-" <> tshow i)
                    , "severity" Aeson..= ("warning" :: Text)
                    ]
            , "annotations" Aeson..= Aeson.object ["summary" Aeson..= ("summary " <> tshow i)]
            , "startsAt" Aeson..= ("2026-09-25T00:00:00Z" :: Text)
            , "generatorURL" Aeson..= ("http://grafana/" <> tshow i)
            ]

freePort :: IO Int
freePort = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    port <- Socket.socketPort sock
    Socket.close sock
    pure (fromIntegral port)
