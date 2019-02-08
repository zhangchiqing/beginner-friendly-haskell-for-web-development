module SimpleService.Routes.UserSpec where

import Control.Monad (void)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
  ( Manager
  , Request(..)
  , RequestBody
  , Response
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types.Status (statusCode)
import SimpleService.Config (Config(..), readConfig)
import SimpleService.Test.Setup (removeAll, startService)
import SimpleService.Types.User (User(..))
import Test.Hspec (Spec, before, beforeAll_, describe, it, shouldBe)

getWithURL :: String -> String -> IO (Response BL.ByteString)
getWithURL appHost url = do
  request :: Request <- parseRequest $ appHost ++ url
  manager :: Manager <- getGlobalManager
  httpLbs request manager

deleteWithURL :: String -> String -> IO (Response BL.ByteString)
deleteWithURL appHost url = do
  req :: Request <- parseRequest $ appHost ++ url
  let request :: Request = req {method = "DELETE"}
  manager :: Manager <- getGlobalManager
  httpLbs request manager

postWithURL :: String -> String -> RequestBody -> IO (Response BL.ByteString)
postWithURL appHost url reqBody = do
  req :: Request <- parseRequest $ appHost ++ url
  let request :: Request = req {method = "POST", requestBody = reqBody}
  manager :: Manager <- getGlobalManager
  httpLbs request manager

localhostFromConfig :: Config -> String
localhostFromConfig config = "http://127.0.0.1:" ++ appPort
  where
    appPort :: String
    appPort = show $ cfgPort config

spec :: Spec
spec =
  beforeAll_ (void startService) $
  before (removeAll >> readConfig) $
  describe "SimpleService.Routes.UserSpec" $ do
    it "POST /users should create a user" $ \config -> do
      let appHost :: String = localhostFromConfig config
      let reqBody :: RequestBody =
            "{\"name\":\"alice\", \"email\":\"alice@example.org\"}"
      resp :: Response BL.ByteString <- postWithURL appHost "/users" reqBody
      let maybeUser :: Maybe User = decode (responseBody resp)
      userName <$> maybeUser `shouldBe` Just "alice"
      userEmail <$> maybeUser `shouldBe` Just "alice@example.org"
    it "POST /users should not create a user if email is missing" $ \config -> do
      let appHost :: String = localhostFromConfig config
      let reqBody :: RequestBody = "{\"name\":\"alice\"}"
      resp :: Response BL.ByteString <- postWithURL appHost "/users" reqBody
      statusCode (responseStatus resp) `shouldBe` 400
    it "POST /users should not create a user if name is missing" $ \config -> do
      let appHost :: String = localhostFromConfig config
      let reqBody :: RequestBody = "{\"email\":\"alice@example.org\"}"
      resp :: Response BL.ByteString <- postWithURL appHost "/users" reqBody
      statusCode (responseStatus resp) `shouldBe` 400
    it "GET /user?name=:name should return the created user" $ \config -> do
      let appHost :: String = localhostFromConfig config
      let reqBody :: RequestBody =
            "{\"name\":\"alice\", \"email\":\"alice@example.org\"}"
      postResp :: Response BL.ByteString <- postWithURL appHost "/users" reqBody
      getResp :: Response BL.ByteString <- getWithURL appHost "/user?name=alice"
      responseStatus postResp `shouldBe` responseStatus getResp
      responseBody postResp `shouldBe` responseBody getResp
    it "GET /user?name=:name should return 404 if user doesn't exist" $ \config -> do
      let appHost :: String = localhostFromConfig config
      resp :: Response BL.ByteString <- getWithURL appHost $ "/user?name=bob"
      statusCode (responseStatus resp) `shouldBe` 404
    it "DELETE /user/:name should delete user" $ \config -> do
      let appHost :: String = localhostFromConfig config
      resp :: Response BL.ByteString <- deleteWithURL appHost "/user/alice"
      statusCode (responseStatus resp) `shouldBe` 204
