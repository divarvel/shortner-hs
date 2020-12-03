module DB
  ( getUrlByCode
  --, addUrl
  , listUrls
  --, removeUrl
  , withPool
  , mkPool
  , nextText
  --, mkUrlEntry
  , getLastCode
  ) where

import           Data.Pool                    (Pool, createPool, withResource)
import qualified Data.Text                    as T
--import           Data.UUID.V4                 (nextRandom)
import           Database.PostgreSQL.Simple   (Connection, Only (..), Query,
                                               close, connectPostgreSQL)
import           Database.PostgreSQL.Transact (DBT, query, runDBTSerializable)

import           Types

mkPool :: Text
       -> IO (Pool Connection)
mkPool conString =
  let mkCon = connectPostgreSQL (encodeUtf8 conString)
      stripes = 1
      timeout = 10
      consPerStripe = 4
   in createPool
        mkCon
        close
        stripes
        timeout
        consPerStripe

withPool :: MonadIO m
         => DBT IO a
         -> ReaderT (Pool Connection) m a
withPool transaction = do
  pool <- ask
  lift $ liftIO $ withResource pool $ runDBTSerializable transaction

selectUrlQuery :: Query
selectUrlQuery = "select url.url_id, url.code, url.long_url, url.hits, url.domain_id, url.is_custom from url"

getUrlByCode :: MonadIO m
             => Text
             -> Text
             -> DBT m (Maybe UrlEntry)
getUrlByCode code domain_id =
  let q = selectUrlQuery <> " where url.code = ? and url.domain_id = ?"
   in fmap listToMaybe . query q $ (code, domain_id)

{-
mkUrlEntry :: MonadIO m
           => DomainId
           -> AbsoluteURL
           -> Maybe Text
           -> DBT m UrlEntry
mkUrlEntry ueDomainId ueLongUrl _mCode = do
  ueId <- UrlId <$> liftIO nextRandom
  pure UrlEntry{..}
-}

getLastCode :: MonadIO m
            => DomainId
            -> DBT m (Maybe Text)
getLastCode =
  let q = "select url.code where url.domain_id = ? order by url.code desc limit 1"
   in fmap (listToMaybe . fmap fromOnly) . query q . Only

nextText :: Text -> Text
nextText t = case T.unsnoc t of
  Nothing            -> "a"
  Just (before, 'z') -> nextText before <> "a"
  Just (before, c)   -> T.snoc before (succ c)

{-
addUrl :: AbsoluteURL -> Maybe Text -> DBT m UrlEntry
addUrl = undefined
-}

listUrls :: MonadIO m
         => DomainId
         -> DBT m [UrlEntry]
listUrls =
  let q = selectUrlQuery <> " where url.domain_id = ?"
   in query q . Only

{-
removeUrl :: Text -> DBT m ()
removeUrl = undefined
-}
