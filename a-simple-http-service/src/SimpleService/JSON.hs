{-# LANGUAGE FlexibleContexts #-}

module SimpleService.JSON
  ( module SimpleService.JSON
  , ToJSON
  , FromJSON
  , parseJSON
  , toJSON
  , Generic
  ) where

import Data.Aeson
  ( FromJSON
  , Options
  , ToJSON
  , genericParseJSON
  , genericToJSON
  , parseJSON
  , toJSON
  )
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Types
  ( GFromJSON
  , GToJSON
  , Parser
  , Value
  , Zero
  , defaultOptions
  , fieldLabelModifier
  )
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic, Rep)

dropPrefix :: String -> (String -> String) -> Options
dropPrefix prefix f = defaultOptions {fieldLabelModifier = f . strip}
  where
    strip str = fromMaybe str $ stripPrefix prefix str

genericToJSONByPrefix ::
     (GToJSON Zero (Rep a), Generic a) => String -> a -> Value
genericToJSONByPrefix prefix = genericToJSON $ dropPrefix prefix snakeCase

genericParseJSONByPrefix ::
     (GFromJSON Zero (Rep a), Generic a) => String -> Value -> Parser a
genericParseJSONByPrefix prefix = genericParseJSON $ dropPrefix prefix snakeCase
