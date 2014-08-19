{-# LANGUAGE TemplateHaskell #-}
module Persist
  ( module Persist
  , module Persist.Schema
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           DBus
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp

import           Persist.Schema
import           Types

getCredentials :: MonadIO m => PSM m (Maybe HostCredentials)
getCredentials = runDB $ fmap entityVal
                <$> selectFirst [] [Desc HostCredentialsChanged]


getCredentials' :: MonadIO m => PSM m (Text, Text)
getCredentials' = do
    mbCred <- getCredentials
    case mbCred of
     Nothing -> liftIO . Ex.throwIO $ MsgError "org.pontarius.Error.GetCredentials"
                                               (Just $ "Credentials not set")
                                               []
     Just cred -> return (cred ^. hostnameL, cred ^. usernameL)

setCredentials :: MonadIO m => Text -> Xmpp.Username -> Xmpp.Password -> PSM m ()
setCredentials hostName username password = runDB $ do
    deleteWhere ([] :: [Filter HostCredentials])
    now <- liftIO getCurrentTime
    _ <- insert $ HostCredentials hostName username password now
    return ()

addIdentity :: MonadIO m =>
                 Text
              -> KeyID
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> PSM m ()
addIdentity keyBackend keyID keyCreated keyImported = do
    sk <- getSigningKey
    let isDefault = maybe Active (const Inactive) sk
    _ <- runDB . insert $ PrivIdent keyBackend keyID Nothing keyCreated keyImported
                     isDefault
    return ()

setSigningKey :: MonadIO m => Text -> KeyID -> PSM m ()
setSigningKey backend keyID = runDB $ do
    updateWhere [ PrivIdentIsDefault ==. Active] [PrivIdentIsDefault =. Inactive]
    updateWhere [ PrivIdentKeyBackend ==. backend, PrivIdentKeyID ==. keyID]
                [PrivIdentIsDefault =. Active]
    return ()

getSigningKey :: MonadIO m => PSM m (Maybe PrivIdent)
getSigningKey = runDB $ do
    fmap entityVal <$> getBy (UniqueDefaultKey Active)

runDB :: MonadIO m => SqlPersistT (LoggingT (ResourceT IO)) b -> PSM m b
runDB f = do
    dbPool <- PSM $ view psDB
    PSM . liftIO . runResourceT . runStderrLoggingT . flip runSqlPool dbPool $ f

xmppRef :: Monad m => PSM m (TMVar Xmpp.Session)
xmppRef = PSM $ view psXmppCon
