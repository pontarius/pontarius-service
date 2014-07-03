{-# LANGUAGE TemplateHaskell #-}
module Persist
  ( module Persist
  , module Persist.Schema
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp

import           Persist.Schema
import           Types

getCredentials :: MonadIO m => PSM m (Maybe HostCredentials)
getCredentials = runDB $
    fmap entityVal <$> selectFirst [] [Desc HostCredentialsChanged]

setCredentials :: MonadIO m => Text -> Xmpp.Username -> Xmpp.Password -> PSM m ()
setCredentials hostName username password = runDB $ do
    deleteWhere ([] :: [Filter HostCredentials])
    now <- liftIO getCurrentTime
    _ <- insert $ HostCredentials hostName username password now
    return ()

getHostname :: (MonadIO m, Functor m) => PSM m Text
getHostname = fromMaybe "" . fmap (view hostnameL) <$> getCredentials

getUsername :: (MonadIO m, Functor m) => PSM m Text
getUsername = fromMaybe "" . fmap (view hostnameL) <$> getCredentials

modifyCredentials :: MonadIO m =>
                     ( HostCredentialsGeneric SqlBackend
                      -> HostCredentialsGeneric backend0)
                  -> PSM m ()
modifyCredentials f = do
    mbCred <- getCredentials
    cred <- case mbCred of
        Nothing -> do
            now <- liftIO getCurrentTime
            return $ HostCredentials "" "" "" now
        Just c -> return c
    let cred' = f cred
    setCredentials (view hostnameL cred') (view usernameL cred')
                   (view passwordL cred')
    return ()

setHostname :: MonadIO m => Text -> PSM m ()
setHostname hn = modifyCredentials (hostnameL .~ hn)

setUsername :: MonadIO m => Text -> PSM m ()
setUsername un = modifyCredentials (usernameL .~ un)

setPassword :: MonadIO m => Text -> PSM m ()
setPassword pw = modifyCredentials (passwordL .~ pw)


addPrivateKey :: MonadIO m =>
                 Text
              -> KeyID
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> PSM m ()
addPrivateKey keyBackend keyID keyCreated keyImported = do
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
