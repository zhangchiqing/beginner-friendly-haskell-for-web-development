module SimpleService.Error.UserError where

import Data.Text (Text)

data UserError
  = UserErrorNameTaken Text
  | UserNotFound
  deriving (Eq, Show)
