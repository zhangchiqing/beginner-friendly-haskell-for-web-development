module SimpleService.Test.Setup where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query(..))
import SimpleService (start)
import SimpleService.Config (cfgDBConnString, readConfig)
import SimpleService.DB.Connect (PoolConnection, acquirePool)
import Test.Hspec.Core.Hooks (around_)
import Test.Hspec.Core.Spec (SpecWith)

truncateTable :: ByteString -> PoolConnection -> IO Int64
truncateTable table pool =
  withResource pool $ \conn ->
    execute_ conn $
    Query $ "TRUNCATE TABLE " <> table <> " RESTART IDENTITY CASCADE"

removeAllUsers :: PoolConnection -> IO Int64
removeAllUsers = truncateTable "users"

removeAll :: IO PoolConnection
removeAll = do
  dbURL :: ByteString <- cfgDBConnString <$> readConfig
  removeAll' dbURL

removeAll' :: ByteString -> IO PoolConnection
removeAll' dbURL = do
  pool :: PoolConnection <- acquirePool dbURL
  _ <- removeAllUsers pool
  return pool

-- starts a scotty server
withSimpleService :: IO () -> IO ()
withSimpleService action = bracket startService stopService (const action)

aroundServerStart :: SpecWith a -> SpecWith a
aroundServerStart = around_ withSimpleService

startService :: IO ThreadId
startService = do
  threadId :: ThreadId <- forkIO start
  putStrLn "Waiting for server ..."
  threadDelay 100000
  return threadId

stopService :: ThreadId -> IO ()
stopService = killThread
