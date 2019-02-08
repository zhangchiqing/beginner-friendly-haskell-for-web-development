module SimpleService where

import SimpleService.Config (cfgDBConnString, readConfig)
import SimpleService.DB.Connect (acquirePool, migrateDb)
import qualified SimpleService.HTTP.Server as Server
import SimpleService.System.Log (configGlobal)

start :: IO ()
start = do
  configGlobal
  config <- readConfig
  pool <- acquirePool (cfgDBConnString config)
  migrateDb pool
  Server.start config pool
