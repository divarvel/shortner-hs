{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API
  ( API (..)
  , ManagementAPI (..)
  , WithUrl (..)
  , UrlEntry (..)
  , PostUrlRequest (..)
  , AbsoluteURL(..)
  ) where

import           Data.Aeson          (FromJSON (..), ToJSON)
import           Servant
import           Servant.API.Generic

import           Types

data API m = API
  { redirect      :: m :- Capture "code" Text :> Get '[JSON] NoContent
  , managementApi :: m :- BasicAuth "URL Management" () :> "api" :> "urls" :> ToServantApi ManagementAPI
  }
  deriving (Generic)

data PostUrlRequest
  = PostUrlRequest
  { code    :: Maybe Text
  , longUrl :: AbsoluteURL
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ManagementAPI m = ManagementAPI
  { getAllUrls :: m :- Get '[JSON] [UrlEntry]
  , postUrl    :: m :- ReqBody '[JSON] PostUrlRequest :> Post '[JSON] UrlEntry
  , withUrl    :: m :- Capture "url" Text :> ToServantApi WithUrl
  }
  deriving (Generic)

data WithUrl m = WithUrl
  { getUrl    :: m :- Get '[JSON] UrlEntry
  , deleteUrl :: m :- Delete '[JSON] NoContent
  }
  deriving (Generic)
