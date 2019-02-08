module SimpleService.Error.ErrorMessage where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- internal type that only used by the error handler
-- return the error message as json { "err" : xxx }
newtype ErrorMessage = ErrorMessage
  { err :: Text
  } deriving (Show, Generic)

instance ToJSON ErrorMessage

instance FromJSON ErrorMessage
