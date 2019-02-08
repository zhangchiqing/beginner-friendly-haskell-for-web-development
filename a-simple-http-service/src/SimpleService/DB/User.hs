module SimpleService.DB.User where

import Control.Exception (handle, throw)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..), SqlError(..), execute, query)
import Database.PostgreSQL.Simple.Errors
  ( ConstraintViolation(..)
  , constraintViolation
  )
import SimpleService.DB.Connect (PoolConnection)
import SimpleService.Error.UnknownException (onlyOne)
import SimpleService.Error.UserError (UserError(..))
import SimpleService.Types.CreateUser (CreateUser, cuEmail, cuName)
import SimpleService.Types.User (User)

createUser :: PoolConnection -> CreateUser -> IO (Either UserError User)
createUser pool cu =
  handle handleError $ (Right . onlyOneUserCreated) <$> createUser' pool cu
  where
    onlyOneUserCreated :: [User] -> User
    onlyOneUserCreated = onlyOne "SimpleService.DB.createUser"
    userName :: Text
    userName = cuName cu
    handleError :: SqlError -> IO (Either UserError User)
    handleError = handler userName

handler :: Text -> SqlError -> IO (Either UserError a)
handler userName sqlError =
  case constraintViolation sqlError of
    Nothing -> throw sqlError
    Just violation ->
      if violation == UniqueViolation "users_name_key"
        then return $ Left $ UserErrorNameTaken userName
        else throw sqlError

createUser' :: PoolConnection -> CreateUser -> IO [User]
createUser' pool cu =
  withResource pool $ \conn -> query conn sql (userName, userEmail)
  where
    sql =
      "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, created_at, name, email"
    userEmail = cuEmail cu
    userName = cuName cu

getUserByName :: PoolConnection -> Text -> IO (Maybe User)
getUserByName pool userName = do
  results <- withResource pool $ \conn -> query conn sql (Only userName)
  case results of
    [user] -> return $ Just user
    _ -> return Nothing
  where
    sql = "SELECT id, created_at, name, email FROM users WHERE name = ?"

deleteUserByName :: PoolConnection -> Text -> IO ()
deleteUserByName pool userName = do
  _ <- withResource pool $ \conn -> execute conn sql (Only userName)
  return ()
  where
    sql = "DELETE FROM users WHERE name = ?"

toUserError :: Text -> SqlError -> Maybe UserError
toUserError username sqlError = do
  violation <- constraintViolation sqlError
  if violation == UniqueViolation "users_name_key"
    then Just $ UserErrorNameTaken username
    else Nothing
