{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds     #-}
module Main where

import           Env
import           Network.Wai.Handler.Warp
import           Servant.API.Generic
import           Servant.Server

import qualified API
import qualified DB
import qualified Server


data Config
  = Config
  { pgUri     :: Text
  , basicAuth :: (Text, Text)
  , port      :: Int
  }

configParser :: Parser Error Config
configParser = do
  pgUri    <- var (str <=< nonempty) "POSTGRESQL_ADDON_URI"  (help "Postgres URI")
  user     <- var (str <=< nonempty) "ADMIN_USER"  (help "Basic auth user for admin tasks")
  password <- var (str <=< nonempty) "ADMIN_PASSWORD"  (help "Basic auth password for admin tasks")
  port     <- var auto "PORT" (help "Port to listen on")
  pure Config{basicAuth=(user,password),..}

main :: IO ()
main = do
  config <- Env.parse (header "Shortner") configParser
  runServer config

ctx :: Config -> Context Ctx
ctx Config{..} = uncurry Server.checkBasicAuth basicAuth :. EmptyContext

type Ctx = '[BasicAuthCheck ()]

runServer :: Config -> IO ()
runServer c@Config{..} = do
  pool <- DB.mkPool pgUri
  let api = genericApi @API.API Proxy
      server = hoistServerWithContext
        api
        (Proxy @Ctx)
        (Server.injectEnv pool)
        Server.server
      app = serveWithContext api (ctx c) server
  run port app
