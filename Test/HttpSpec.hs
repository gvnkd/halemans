module Test.HttpSpec where

import Application.Service.Http (HttpStatusError (..), getFollowing, getFollowingStream, isDeterministicClientError, postFollowing)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((&), (.~), (^.))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import IHP.Prelude
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, status302, status403, statusCode)
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import Test.Hspec

-- Redirect-following HTTP wrapper: a local warp server 302s /start (absolute,
-- different host spelling), /rel (relative) and /loop (self); /real records
-- the request and answers 200 JSON.
spec :: Spec
spec = describe "Application.Service.Http" do
    around withServer do
        it "follows a cross-host 302 on POST, re-sending Authorization" \(baseUrl, seen) -> do
            response <-
                postFollowing
                    (Wreq.defaults & Wreq.header "Authorization" .~ ["Bearer sekret"])
                    (baseUrl <> "/start")
                    (Aeson.object ["ping" Aeson..= True])
            statusCodeOf response `shouldBe` 200
            requests <- readIORef seen
            let real = fromJust (find (\req -> Wai.pathInfo req == ["real"]) requests)
            lookup "Authorization" (Wai.requestHeaders real) `shouldBe` Just "Bearer sekret"

        it "resolves relative Location against the current URL" \(baseUrl, _) -> do
            response <- getFollowing Wreq.defaults (baseUrl <> "/rel")
            statusCodeOf response `shouldBe` 200

        it "throws HttpStatusError on a non-2xx final response" \(baseUrl, _) -> do
            getFollowing Wreq.defaults (baseUrl <> "/forbidden")
                `shouldThrow` \case
                    HttpStatusError _ code -> code == 403

        it "caps redirect hops and reports the last redirect status" \(baseUrl, _) -> do
            getFollowing Wreq.defaults (baseUrl <> "/loop")
                `shouldThrow` \case
                    HttpStatusError _ code -> code == 302

        it "returns the raw response when the caller set a custom checkResponse" \(baseUrl, _) -> do
            let opts = Wreq.defaults & Wreq.checkResponse .~ Just (\_ _ -> pure ())
            response <- getFollowing opts (baseUrl <> "/forbidden")
            statusCodeOf response `shouldBe` 403

        it "streams a redirect-following GET, re-sending Authorization" \(baseUrl, seen) -> do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            body <-
                getFollowingStream
                    manager
                    (baseUrl <> "/start")
                    [("Authorization", "Bearer sekret")]
                    \response -> cs . LBS.fromChunks <$> HTTP.brConsume (HTTP.responseBody response)
            (body :: Text) `shouldBe` "{\"ok\":true}"
            requests <- readIORef seen
            let real = fromJust (find (\req -> Wai.pathInfo req == ["real"]) requests)
            lookup "Authorization" (Wai.requestHeaders real) `shouldBe` Just "Bearer sekret"

        it "throws HttpStatusError on a non-2xx streaming GET" \(baseUrl, _) -> do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            getFollowingStream manager (baseUrl <> "/forbidden") [] (\_ -> pure ())
                `shouldThrow` \case
                    HttpStatusError _ code -> code == 403

    describe "isDeterministicClientError" do
        it "flags 4xx client errors as not worth retrying" do
            isDeterministicClientError (tshow (HttpStatusError "http://x/rest/api/3/search" 400)) `shouldBe` True
            isDeterministicClientError (tshow (HttpStatusError "http://x/rest/api/3/search" 403)) `shouldBe` True
            isDeterministicClientError (tshow (HttpStatusError "http://x/confluence/rest/api/content" 404)) `shouldBe` True
        it "keeps 408/429, 5xx and non-http errors retryable" do
            isDeterministicClientError (tshow (HttpStatusError "http://x/" 408)) `shouldBe` False
            isDeterministicClientError (tshow (HttpStatusError "http://x/" 429)) `shouldBe` False
            isDeterministicClientError (tshow (HttpStatusError "http://x/" 502)) `shouldBe` False
            isDeterministicClientError "Connection refused" `shouldBe` False

statusCodeOf :: Wreq.Response body -> Int
statusCodeOf response = statusCode (response ^. Wreq.responseStatus)

withServer :: ((String, IORef [Wai.Request]) -> IO ()) -> IO ()
withServer action = do
    port <- freePort
    seen <- newIORef []
    ready <- newEmptyMVar
    let settings = Warp.setPort port (Warp.setBeforeMainLoop (putMVar ready ()) Warp.defaultSettings)
    _ <- forkIO (Warp.runSettings settings (serverApp seen port))
    takeMVar ready
    action ("http://localhost:" ++ cs (show port), seen)

serverApp :: IORef [Wai.Request] -> Int -> Wai.Application
serverApp seen port req respond = do
    modifyIORef' seen (req :)
    respond case Wai.pathInfo req of
        ["start"] -> redirect ("http://127.0.0.1:" ++ cs (show port) ++ "/real")
        ["rel"] -> redirect "/real"
        ["loop"] -> redirect "/loop"
        ["forbidden"] -> Wai.responseLBS status403 [] "denied"
        _ -> Wai.responseLBS status200 [("Content-Type", "application/json")] "{\"ok\":true}"
  where
    redirect location = Wai.responseLBS status302 [("Location", BC.pack location)] ""

freePort :: IO Int
freePort = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    port <- Socket.socketPort sock
    Socket.close sock
    pure (fromIntegral port)
