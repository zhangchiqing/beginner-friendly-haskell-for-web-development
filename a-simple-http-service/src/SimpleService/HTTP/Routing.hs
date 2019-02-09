module SimpleService.HTTP.Routing where

import Network.HTTP.Types (status404)
import SimpleService.Config (Config(..))
import SimpleService.DB.Connect (PoolConnection)
import SimpleService.HTTP.Action.Ping (ping)
import SimpleService.HTTP.Action.User (createUser, deleteUser, getUser)
import Web.Scotty (ScottyM, delete, get, html, notFound, post, status)

setup :: Config -> PoolConnection -> ScottyM ()
setup config pool = do
  routing config pool
  notFound $ do
    status status404
    html "Not found"

routing :: Config -> PoolConnection -> ScottyM ()
routing _ pool = do
  get "/ping" $ ping
  post "/users" $ createUser pool
  get "/user" $ getUser pool
  delete "/user/:name" $ deleteUser pool
