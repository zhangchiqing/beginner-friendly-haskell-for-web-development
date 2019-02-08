module SimpleService.Types.CreateUser where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , (.:)
  , (.=)
  , object
  , parseJSON
  , toJSON
  )
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data CreateUser = CreateUser
  { cuName :: Text
  , cuEmail :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON CreateUser where
  parseJSON (Object obj) = CreateUser <$> obj .: "name" <*> obj .: "email"
  parseJSON invalid = typeMismatch "CreateUser" invalid

instance ToJSON CreateUser where
  toJSON (CreateUser name email) = object ["name" .= name, "email" .= email]

instance FromRow CreateUser
