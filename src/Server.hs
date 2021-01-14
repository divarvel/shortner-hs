module Server
  ( server
  , injectEnv
  , checkBasicAuth
  ) where

import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types.Header  (hLocation)
import           Servant                    hiding (Handler, Server)
import qualified Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           API
import           DB

type Handler = ReaderT (Pool Connection) Servant.Handler
type Server api = ServerT api Handler

injectEnv :: Pool Connection
          -> Handler a
          -> Servant.Handler a
injectEnv = flip runReaderT

server :: Server (ToServantApi API)
server = genericServerT $ API
 { redirectRoot  = redirectEndpoint ""
 , redirect      = redirectEndpoint
 , managementApi = \() -> managementApiServer
 }

managementApiServer :: Server (ToServantApi ManagementAPI)
managementApiServer = genericServerT $ ManagementAPI
  { getAllUrls = getAllUrlsEndpoint
  , postUrl = postUrlEndpoint
  , withUrl = withUrlServer
  }

withUrlServer :: Text -> Server (ToServantApi WithUrl)
withUrlServer urlCode = genericServerT $ WithUrl
  { getUrl = getUrlEndpoint urlCode
  , deleteUrl = deleteUrlEndpoint urlCode
  }

or404 :: Maybe a -> Handler a
or404 = maybe (throwError err404) pure

notImplemented :: Handler a
notImplemented = throwError err501

redirectEndpoint :: Text -> Handler NoContent
redirectEndpoint code =
  let redirect UrlEntry{ueLongUrl = AbsoluteURL url} = throwError $
        err301 { errHeaders = [(hLocation, encodeUtf8 url)] }
   in redirect =<< or404 =<< withPool (getUrlByCode code "aecfe336-b0d6-4074-8d49-91d9e98c5eb3")

getAllUrlsEndpoint :: Handler [UrlEntry]
getAllUrlsEndpoint = notImplemented -- withPool $ listUrls undefined

postUrlEndpoint :: PostUrlRequest -> Handler UrlEntry
postUrlEndpoint _ = notImplemented
{-
PostUrlRequest{..} =
  withPool $ addUrl longUrl code
  -}

getUrlEndpoint :: Text -> Handler UrlEntry
getUrlEndpoint _code = notImplemented
   --or404 =<< withPool (getUrlByCode code)

deleteUrlEndpoint :: Text -> Handler NoContent
deleteUrlEndpoint _code =
   notImplemented -- NoContent <$ withPool (removeUrl code)

checkBasicAuth :: Text -> Text -> BasicAuthCheck ()
checkBasicAuth u p = BasicAuthCheck $ \basicAuthData ->
  let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
  in pure $ if u == username && p == password
            then Authorized ()
            else NoSuchUser
