module SimpleService.Error.UnknownException where

import Control.Exception (Exception, throw)
import Data.Text (Text)

newtype UnknownException = UnknownException
  { caller :: Text
  } deriving (Show)

instance Exception UnknownException

onlyOne :: Text -> [a] -> a
onlyOne _ [x] = x
onlyOne fn _ = throw $ UnknownException fn
