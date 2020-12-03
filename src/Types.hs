module Types
  ( AbsoluteURL (..)
  , UrlEntry (..)
  , DomainId (..)
  , UrlId (..)
  ) where

import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), withText)
import qualified Data.Text                            as T
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple           (FromRow, ToRow)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           Network.URL                          (Host (..), Protocol (..),
                                                       URL (..), URLType (..),
                                                       importURL)

newtype DomainId = DomainId { getDomainId :: UUID }
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromField, ToField)

newtype UrlId = UrlId { getUrlId :: UUID }
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromField, ToField)

data UrlEntry = UrlEntry
  { ueId       :: UrlId
  , ueCode     :: Text
  , ueLongUrl  :: AbsoluteURL
  , ueHits     :: Int
  , ueDomainId :: DomainId
  , ueIsCustom :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, FromJSON, ToJSON)

newtype AbsoluteURL =
  AbsoluteURL Text
    deriving newtype (Eq, Show, ToJSON, FromField, ToField)

parseAbsoluteURL :: Text -> Maybe AbsoluteURL
parseAbsoluteURL t = do
  URL{url_type} <- importURL $ T.unpack t
  Absolute Host{protocol = HTTP _} <- Just url_type
  pure $ AbsoluteURL t


instance FromJSON AbsoluteURL where
  parseJSON = withText "HTTP or HTTPS url" $ maybe (fail "The URL must be a HTTP(s) URL") pure . parseAbsoluteURL

