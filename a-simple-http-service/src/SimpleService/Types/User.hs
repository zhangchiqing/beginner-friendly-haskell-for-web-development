module SimpleService.Types.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data User = User
  { userId :: UUID
  , userCreatedAt :: UTCTime
  , userName :: Text
  , userEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance FromRow User
