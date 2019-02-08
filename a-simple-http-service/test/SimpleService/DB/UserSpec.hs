module SimpleService.DB.UserSpec where

import Control.Exception (throwIO)
import SimpleService.DB.User (createUser, deleteUserByName, getUserByName)
import SimpleService.Error.UserError (UserError(..))
import SimpleService.Test.Setup (removeAll)
import SimpleService.Types.CreateUser (CreateUser(..))
import SimpleService.Types.User (User(..))
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe)

testCreateUser :: CreateUser
testCreateUser = CreateUser "alice" "alice@example.org"

userToCreateUser :: User -> CreateUser
userToCreateUser (User _ _ name email) = CreateUser name email

shouldBeRight :: (Show a) => Either a b -> IO b
shouldBeRight (Left a) =
  throwIO $ concat ["should be Right value but isn't, because:", show a]
shouldBeRight (Right b) = return b

shouldBeJust :: Maybe a -> IO a
shouldBeJust Nothing = throwIO "should be Just value but is nothing"
shouldBeJust (Just a) = return a

spec :: Spec
spec =
  before removeAll $
  describe "SimpleService.DB.UserSpec" $ do
    it "should create" $ \pool -> do
      eitherUser :: Either UserError User <- createUser pool testCreateUser
      userToCreateUser <$> eitherUser `shouldBe` Right testCreateUser
    it "should get nothing if not created" $ \pool -> do
      maybeUser :: Maybe User <- getUserByName pool "alice"
      maybeUser `shouldBe` Nothing
    it "should get nothing if not exist" $ \pool -> do
      _ <- createUser pool testCreateUser
      maybeUser :: Maybe User <- getUserByName pool "bob"
      maybeUser `shouldBe` Nothing
    it "should create then get" $ \pool -> do
      eitherUser :: Either UserError User <- createUser pool testCreateUser
      maybeUser :: Maybe User <- getUserByName pool "alice"
      userCreated :: User <- shouldBeRight eitherUser
      userGot :: User <- shouldBeJust maybeUser
      userCreated `shouldBe` userGot
    it "should not be created again with the same name" $ \pool -> do
      _ <- createUser pool testCreateUser
      eitherUser :: Either UserError User <- createUser pool testCreateUser
      eitherUser `shouldBe` Left (UserErrorNameTaken "alice")
    it "should get nothing after being deleted" $ \pool -> do
      _ <- createUser pool testCreateUser
      deleteUserByName pool "alice"
      maybeUser <- getUserByName pool "alice"
      maybeUser `shouldBe` Nothing
    it "should delete non-exist" $ \pool -> deleteUserByName pool "alice"
