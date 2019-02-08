module SimpleService.HTTP.Headers where

import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.Wai (Request, requestHeaders)
import Web.Scotty (ActionM, header)

type RequestId = TL.Text

hRequestId :: HeaderName
hRequestId = "X-Request-Id"

requestIdHeader :: TL.Text
requestIdHeader = "x-request-id"

findHeader :: HeaderName -> [Header] -> Maybe Header
findHeader name = find (headerMatch name)

headerMatch :: HeaderName -> Header -> Bool
headerMatch headerName (name, _) = headerName == name

findRequestIdFromRequest :: Request -> Maybe ByteString
findRequestIdFromRequest req =
  snd <$> findHeader hRequestId (requestHeaders req)

-- | maybe find the requestId from the header
findRequestIdFromHeader :: ActionM (Maybe RequestId)
findRequestIdFromHeader = header requestIdHeader

requestIdFromHeader :: ActionM RequestId
requestIdFromHeader = do
  mh <- findRequestIdFromHeader
  case mh of
    Nothing -> return "no request id"
    Just rid -> return rid
