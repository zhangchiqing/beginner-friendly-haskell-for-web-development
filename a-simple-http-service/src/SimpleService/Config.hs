module SimpleService.Config where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import System.Environment (getEnv)
import Text.Read (readMaybe)

data Config = Config
  { cfgPort :: Int
  , cfgSecret :: String
  , cfgDBConnString :: ByteString
  } deriving (Show)

parseEnv :: (String -> Maybe a) -> String -> IO a
parseEnv maybeParse envvar = do
  v <- getEnv envvar
  case maybeParse v of
    Nothing -> error $ "Can not parse envvar: " ++ evname
    Just parsed -> return parsed
  where
    evname = "the Environment Variable: " ++ envvar

parseInt :: String -> IO Int
parseInt = parseEnv readMaybe

parseString :: String -> IO String
parseString = parseEnv Just

parseByteString :: String -> IO ByteString
parseByteString = parseEnv $ Just . pack

readConfig :: IO Config
readConfig = do
  port <- parseInt "PORT"
  secret <- parseString "SECRET"
  dbConnString <- parseByteString "DB_CONN_STRING"
  return $ Config port secret dbConnString
