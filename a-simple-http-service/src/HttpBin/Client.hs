module HttpBin.Client where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (statusCode)

-- introduce Wreq
-- introduce qualified
import qualified Network.Wreq as W

fetchGet :: IO (W.Response BL.ByteString)
fetchGet = W.get "https://httpbin.org/get"

getResponseCode :: IO Int
getResponseCode = do
  resp <- fetchGet -- W.Response BL.ByteString
  let status = resp ^. W.responseStatus -- W.Status
  let code = statusCode status -- Int
  return code -- IO Int

getMyIP :: IO Text
getMyIP = do
  resp <- fetchGet -- W.Response BL.ByteString
  let mid = resp ^. W.responseBody . key "origin" . _String -- Text
  return mid -- IO Text

--
-- Types and JSON
--
--
data GetReq = GetReq
  { origin :: Text
  , url :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GetReq

instance FromJSON GetReq

--
-- > fetchGetReq
-- Just (GetReq {origin = "75.156.79.232", url = "https://httpbin.org/get"})
fetchGetReq :: IO (Maybe GetReq)
fetchGetReq = do
  resp <- fetchGet
  let body = resp ^. W.responseBody -- BL.ByteString
  let maybeGetReq = decode body -- Maybe GetReq
  return maybeGetReq

sendPost :: IO (W.Response BL.ByteString)
sendPost = W.post "https://httpbin.org/post" dataToPost
  where
    dataToPost = encode $ GetReq "10.1.1.1" "https://myhaskell.org"

parsePost :: IO Text
parsePost = do
  resp <- sendPost
  let body = resp ^. W.responseBody . key "data" . _String
  return body
