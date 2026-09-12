module Test.PushSpec where

import Application.Service.Push
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import IHP.Prelude
import Test.Hspec
import "cryptonite" Crypto.Hash (SHA256 (..))
import "cryptonite" Crypto.PubKey.ECC.DH (calculatePublic)
import "cryptonite" Crypto.PubKey.ECC.ECDSA (PublicKey (..), Signature (..), verify)
import "cryptonite" Crypto.PubKey.ECC.Types (CurveName (SEC_p256r1), Point (..), getCurveByName)

spec :: Spec
spec = describe "Application.Service.Push" do
    let keys =
            VapidKeys
                { vapidPublicKeyB64Url = ""
                , vapidPrivateScalar = 0x1f3a8b2c
                , vapidSubject = "mailto:test@halemans.local"
                }

    describe "b64url" do
        it "round-trips" do
            b64urlDecode (b64urlEncode ("\x00\x01\x02hello\xff" :: ByteString)) `shouldBe` ("\x00\x01\x02hello\xff" :: ByteString)
        it "handles unpadded input" do
            b64urlDecode "SGVsbG8" `shouldBe` ("Hello" :: ByteString)

    describe "i2osp/os2ip" do
        it "round-trips 32-byte values" do
            os2ip (i2osp 32 0x1f3a8b2c) `shouldBe` 0x1f3a8b2c
        it "pads to fixed length" do
            BS.length (i2osp 32 1) `shouldBe` 32

    describe "point encoding" do
        it "round-trips curve points" do
            let curve = getCurveByName SEC_p256r1
            let point = calculatePublic curve keys.vapidPrivateScalar
            pointFromBytes (pointToBytes point) `shouldBe` Just point

    describe "signVapidJwt" do
        it "produces a JWT verifiable with the VAPID public key" do
            jwt <- signVapidJwt keys "https://push.example.com/send/123"
            let [header, claims, signatureB64] = Text.splitOn "." jwt
            length [header, claims, signatureB64] `shouldBe` 3
            let signatureBytes = b64urlDecode signatureB64
            BS.length signatureBytes `shouldBe` 64
            let (r, s) = BS.splitAt 32 signatureBytes
            let signature = Signature{sign_r = os2ip r, sign_s = os2ip s}
            let curve = getCurveByName SEC_p256r1
            let publicKey = PublicKey{public_curve = curve, public_q = calculatePublic curve keys.vapidPrivateScalar}
            verify SHA256 publicKey signature (cs (header <> "." <> claims) :: ByteString) `shouldBe` True

        it "sets aud to the endpoint origin" do
            jwt <- signVapidJwt keys "https://push.example.com/send/123"
            let [_, claims, _] = Text.splitOn "." jwt
            cs (b64urlDecode claims) `shouldSatisfy` ("https://push.example.com" `Text.isInfixOf`)
