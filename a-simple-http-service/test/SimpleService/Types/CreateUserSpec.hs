module SimpleService.Types.CreateUserSpec where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import SimpleService.Types.CreateUser (CreateUser(..))
import Test.Hspec (Spec, describe, it, shouldBe)

testCreateUser :: CreateUser
testCreateUser = CreateUser "alice" "alice@example.org"

spec :: Spec
spec =
  describe "SimpleService.Types.CreateUserSpec" $ do
    it "should be decoded from json" $ do
      jsonString :: BL.ByteString <- BL8.readFile "./test/json/create_user.json"
      decode jsonString `shouldBe` Just testCreateUser
    it "should be decoded after being encoded" $
      decode (encode testCreateUser) `shouldBe` Just testCreateUser
