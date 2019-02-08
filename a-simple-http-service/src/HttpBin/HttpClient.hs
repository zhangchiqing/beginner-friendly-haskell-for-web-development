module HttpBin.HttpClient where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , (.=)
  , eitherDecode
  , encode
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Strings (lazyByteStringToString)
import Data.Text (Text)
import Network.HTTP.Client
  ( Request
  , RequestBody(..)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  )
import Network.HTTP.Client.TLS (getGlobalManager)

getReq :: IO ()
getReq = do
  manager <- getGlobalManager
  request <- parseRequest "https://httpbin.org/get"
  response <- httpLbs request manager
  putStrLn "response status is:"
  print $ responseStatus response
  putStrLn ""
  putStrLn "response headers is:"
  print $ responseHeaders response
  putStrLn ""
  putStrLn "response body is:"
  putStrLn $ lazyByteStringToString $ responseBody response

getReqWithToken :: String -> IO ()
getReqWithToken _ = do
  manager <- getGlobalManager
  request <- parseRequest "https://httpbin.org/get"
  let existingHeaders = requestHeaders request
  let newHeaders =
        ("Content-Type", "application/json") :
        ("Authentication", "FAKE-API-TOKEN") : existingHeaders
  response <- httpLbs request {requestHeaders = newHeaders} manager
  putStrLn "response status is:"
  print $ responseStatus response
  putStrLn ""
  putStrLn "response headers is:"
  print $ responseHeaders response
  putStrLn ""
  putStrLn "response body is:"
  putStrLn $ lazyByteStringToString $ responseBody response

data CreateUser = CreateUser
  { createUserName :: Text
  , createUserEmail :: Text
  } deriving (Show)

instance ToJSON CreateUser where
  toJSON (CreateUser name email) = object ["name" .= name, "email" .= email]

postUser :: IO ()
postUser = do
  manager <- getGlobalManager
  request <- (parseRequest "https://httpbin.org/post")
  let body =
        RequestBodyLBS (encode (CreateUser "alice" "alice@example.org")) :: RequestBody
  let requestWithBody = request {requestBody = body, method = "POST"} :: Request
  response <- httpLbs requestWithBody manager
  putStrLn "response status is:"
  print $ responseStatus response
  putStrLn ""
  putStrLn "response headers is:"
  print $ responseHeaders response
  putStrLn ""
  putStrLn "response body is:"
  putStrLn $ lazyByteStringToString $ responseBody response

data User = User
  { userName :: Text
  , userEmail :: Text
  } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User <$> v .: "email" <*> v .: "name"

data PostResponse = PostResponse
  { postResponseJson :: User
  } deriving (Show)

instance FromJSON PostResponse where
  parseJSON = withObject "PostResponse" $ \v -> PostResponse <$> v .: "json"

createUser :: CreateUser -> IO PostResponse
createUser cu = do
  manager <- getGlobalManager
  request <- (parseRequest "https://httpbin.org/post")
  let body = RequestBodyLBS (encode cu) :: RequestBody
  let requestWithBody = request {requestBody = body, method = "POST"} :: Request
  response <- httpLbs requestWithBody manager
  case eitherDecode (responseBody response) of
    Left err -> error err
    Right postResp -> return postResp
