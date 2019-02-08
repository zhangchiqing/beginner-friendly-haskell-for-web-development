module SimpleService.HTTP.Middleware.Logging where

import Data.ByteString as ByteString
import Data.Maybe (fromMaybe)
import Data.Strings (byteStringToString, textToByteString)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
  ( Application
  , Request
  , Response
  , mapResponseHeaders
  , rawPathInfo
  , requestHeaders
  , requestMethod
  , responseStatus
  )
import SimpleService.HTTP.Headers
  ( findHeader
  , findRequestIdFromRequest
  , hRequestId
  )
import SimpleService.System.Log (infoM)

next :: IO ByteString
next = textToByteString . toText <$> nextRandom

-- generate a new random uuid as request id and attach it to the request header.
makeReq :: Request -> IO (Header, Request, ByteString)
makeReq req = do
  rid :: ByteString <- next
  let headers = requestHeaders req :: [Header]
      reqIdHeader = (hRequestId, rid) :: Header
      headers' = reqIdHeader : headers :: [Header]
  case findHeader hRequestId headers of
    Nothing -> return (reqIdHeader, req {requestHeaders = headers'}, rid)
    Just existingHeader -> return (existingHeader, req, rid)

logging :: Request -> ByteString -> ByteString
logging req rid = ByteString.concat [rid, " ", method, " ", path]
  where
    method :: ByteString
    method = requestMethod req
    path :: ByteString
    path = rawPathInfo req

logRequest :: Application -> Application
logRequest app req respond = do
  (header', req', rid) :: (Header, Request, ByteString) <- makeReq req
  infoLog $ byteStringToString $ logging req rid
  app req' $ \res -> do
    let res' = mapResponseHeaders (header' :) res :: Response
    respond res'
  where
    infoLog :: String -> IO ()
    infoLog = infoM "[RequestId]"

-- | Before sending the respond, log the response code and request id
logResponse :: Application -> Application
logResponse app req respond =
  app req $ \res -> do
    let rid = fromMaybe "" $ findRequestIdFromRequest req :: ByteString
        code = statusCode $ responseStatus res :: Int
    infoLog $
      Prelude.concat [byteStringToString rid, " ResponseCode: ", show code]
    respond res
  where
    infoLog :: String -> IO ()
    infoLog = infoM "[Response]"
