module SimpleService.HTTP.Action.Ping where

import SimpleService.Types.Pong (success)
import Web.Scotty (ActionM, json)

ping :: ActionM ()
ping = json success
