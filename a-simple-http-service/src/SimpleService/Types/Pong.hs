module SimpleService.Types.Pong where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Pong = Pong
  { pong :: Bool
  } deriving (Show, Generic)

success :: Pong
success = Pong True

instance ToJSON Pong
