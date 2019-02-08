module Data.User where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , decode
  , encode
  , object
  , parseJSON
  , toJSON
  )
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userIsAdmin :: Bool
  , userInfo :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON User where
  toJSON (User name email age isAdmin info) =
    object $
    [ "name" .= name
    , "email" .= email
    , "age" .= age
    , "isAdmin" .= isAdmin
    , "info" .= toNullable info
    ]
    where
      toNullable Nothing = Null
      toNullable (Just i) = String i

instance FromJSON User where
  parseJSON (Object obj) =
    User <$> obj .: "name" <*> obj .: "email" <*> obj .: "age" <*>
    obj .: "isAdmin" <*>
    obj .:? "info"
  parseJSON invalid = typeMismatch "User" invalid

alice :: User
alice =
  User
    { userName = "alice"
    , userEmail = "alice@example.org"
    , userAge = 20
    , userIsAdmin = False
    , userInfo = Nothing
    }

aliceEncoded :: BL.ByteString
aliceEncoded = encode alice

aliceDecoded :: Maybe User
aliceDecoded = decode aliceEncoded
