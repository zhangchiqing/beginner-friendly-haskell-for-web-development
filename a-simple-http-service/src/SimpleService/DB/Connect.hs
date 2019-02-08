module SimpleService.DB.Connect where

-- this module provides functions to
-- 1. connect database
-- 2. run migration
-- 3. ping the connection
import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , close
  , connectPostgreSQL
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..)
  , MigrationContext(..)
  , MigrationResult(..)
  , runMigration
  )

type PoolConnection = Pool Connection

-- https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html#v:createPool
-- > acquirePool "postgres://postgres@127.0.0.1:7432/simpleservice"
-- Pool {numStripes = 1, idleTime = 10s, maxResources = 10}
acquirePool :: ByteString -> IO PoolConnection
acquirePool pgUrl =
  createPool (connectPostgreSQL pgUrl) close numStripes idleTime maxResources
  where
    numStripes = 1
    idleTime = 10
    maxResources = 10

-- DB migration files is hardcoded to read from the `migrations` folder which is in the same folder as the executable.
migrateDb :: PoolConnection -> IO ()
migrateDb pool =
  withResource pool $ \conn -> do
    result <- withTransaction conn (runMigration (ctx conn))
    case result of
      MigrationError err -> error err
      MigrationSuccess -> return ()
  where
    ctx = MigrationContext cmd False
    cmd =
      MigrationCommands
        [MigrationInitialization, MigrationDirectory "migrations"]

-- Ping DB with a select query
-- > withResource pool ping
-- 2
ping :: Connection -> IO Int
ping conn = do
  [Only i] <- query_ conn "select 1 + 1"
  return i
