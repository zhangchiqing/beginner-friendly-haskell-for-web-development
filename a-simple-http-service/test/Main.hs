module Main where

import SimpleService.Config (Config(..), readConfig)
import SimpleService.DB.Connect (acquirePool, migrateDb)
import Spec (spec)
import Test.Hspec (hspec)

main :: IO ()
main = do
  config <- readConfig
  pool <- acquirePool $ cfgDBConnString config
  migrateDb pool
  hspec spec
