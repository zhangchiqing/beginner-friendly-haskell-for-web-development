module SimpleService.HTTP.Action.User where

import Data.Aeson (FromJSON)
import Data.Strings (lazyTextToText, showLazyText)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (Status, status204, status400, status404)
import SimpleService.DB.Connect (PoolConnection)
import qualified SimpleService.DB.User as User
import SimpleService.Error.ErrorMessage (ErrorMessage(..))
import SimpleService.Error.UserError (UserError(..))
import SimpleService.Types.CreateUser (CreateUser)
import SimpleService.Types.User (User)
import Web.Scotty
  ( ActionM
  , Parsable
  , json
  , jsonData
  , liftAndCatchIO
  , param
  , raise
  , rescue
  , status
  )

createUser :: PoolConnection -> ActionM ()
createUser pool = do
  eitherBody :: Either InvalidJSON CreateUser <- jsonDataAndCatch
  case eitherBody of
    Left e -> errStatus status400 $ lazyTextToText e
    Right body -> do
      eitherUser :: Either UserError User <-
        liftAndCatchIO $ User.createUser pool body
      case eitherUser of
        Left (UserErrorNameTaken e) -> errStatus status400 e
        Left e -> raise $ showLazyText e
        Right user -> json user

type InvalidJSON = TL.Text

jsonDataAndCatch :: FromJSON a => ActionM (Either InvalidJSON a)
jsonDataAndCatch = (Right <$> jsonData) `rescue` (return . Left)

errStatus :: Status -> Text -> ActionM ()
errStatus st msg = do
  status st
  json $ ErrorMessage msg

getUser :: PoolConnection -> ActionM ()
getUser pool = do
  eitherName :: Either TL.Text Text <- paramAndCatch "name"
  case eitherName of
    Left e -> errStatus status400 $ lazyTextToText e
    Right name -> do
      maybeUser :: Maybe User <- liftAndCatchIO $ User.getUserByName pool name
      case maybeUser of
        Nothing -> status status404
        Just user -> json user

type InvalidParam = TL.Text

paramAndCatch :: Parsable a => TL.Text -> ActionM (Either InvalidParam a)
paramAndCatch field = (Right <$> param field) `rescue` (return . Left)

deleteUser :: PoolConnection -> ActionM ()
deleteUser pool = do
  eitherName :: Either TL.Text Text <- paramAndCatch "name"
  case eitherName of
    Left e -> errStatus status400 $ lazyTextToText e
    Right name -> do
      liftAndCatchIO $ User.deleteUserByName pool name
      status status204
