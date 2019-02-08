module SimpleService.Error.Handler where

import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Strings (lazyByteStringToString, lazyTextToString)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status500)
import SimpleService.Error.ErrorMessage (ErrorMessage(..))
import SimpleService.HTTP.Headers (findRequestIdFromHeader)
import SimpleService.System.Log (errorM)
import Web.Scotty (ActionM, body, json, liftAndCatchIO, status)

handleException :: TL.Text -> ActionM ()
handleException e = do
  maybeRequestId :: Maybe TL.Text <- findRequestIdFromHeader
  let rid = fromMaybe "" maybeRequestId :: TL.Text
  reqBody :: LBS.ByteString <- body
  liftAndCatchIO $ errorM "[Error.Handler]" $ buildErrorMessage e rid reqBody
  status status500
  json $ ErrorMessage "Internal Server Error"

buildErrorMessage :: TL.Text -> TL.Text -> LBS.ByteString -> String
buildErrorMessage e rid reqBody =
  concat
    [ lazyTextToString rid
    , ", error: "
    , show e
    , ", request body: " ++ lazyByteStringToString reqBody
    ]
