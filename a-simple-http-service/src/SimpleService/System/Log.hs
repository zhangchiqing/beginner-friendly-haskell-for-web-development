module SimpleService.System.Log where

import Data.Time.Clock (getCurrentTime)
import qualified System.Log.Logger as L

-- config the logging globally so that we only send INFO log and error log to stdout
configGlobal :: IO ()
configGlobal = L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)

type Logger = String -> String -> IO ()

-- print info logs to std out. Example:
--  [2018-05-14 17:46:33.745626 UTC] [INFO] [Service.start] connecting db
infoM :: Logger
infoM = makeLogging "INFO" L.infoM

-- print error logs to std out
errorM :: Logger
errorM = makeLogging "ERROR" L.errorM

noticeM :: Logger
noticeM = makeLogging "NOTICE" L.noticeM

makeLogging :: String -> Logger -> Logger
makeLogging level f method msg = do
  time <- getCurrentTime
  let fullMsg = concat ["[", show time, "] [", level, "] ", method, " ", msg]
  f method fullMsg
