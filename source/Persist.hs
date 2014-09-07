{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Persist
  ( module Persist
  , module Persist.Schema
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Text (Text)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp

import           Persist.Schema
import           Types


setCredentials :: MonadIO m => Text -> Xmpp.Username -> Xmpp.Password -> PSM m ()
setCredentials hostName username password = do
    runDB $ do
        deleteWhere ([] :: [Filter HostCredentials])
        now <- liftIO getCurrentTime
        _ <- insert $ HostCredentials hostName username password now
        return ()

getCredentials :: MonadIO m => PSM m (Maybe HostCredentials)
getCredentials = runDB $ fmap entityVal
                <$> selectFirst [] [Desc HostCredentialsChanged]

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
    _ <- insertUnique (PrivIdent "gpg" keyID Nothing Nothing Nothing Inactive)
    updateWhere [ PrivIdentIsDefault ==. Active] [PrivIdentIsDefault =. Inactive]
    updateWhere [ PrivIdentKeyBackend ==. backend
                , PrivIdentKeyID ==. keyID
                , PrivIdentRevoked ==. Nothing
                ]
                [ PrivIdentIsDefault =. Active]
    return ()

getSigningKey :: MonadIO m => PSM m (Maybe PrivIdent)
getSigningKey = runDB $ do
    fmap entityVal <$> getBy (UniqueDefaultKey Active)

addPubIdent :: MonadIO m => KeyID -> PSM m ()
addPubIdent keyID = do
    now <- liftIO $ getCurrentTime
    _ <- runDB $ insertUnique (PubIdent "gpg" keyID Nothing Nothing now (Just now))
    return ()

associatePubIdent :: MonadIO m => Xmpp.Jid -> KeyID -> PSM m Bool
associatePubIdent peer keyID = runDB $ do
    mbKey <- getBy $ UniquePubIdentKey keyID
    case mbKey of
        Nothing -> return False
        Just key -> do
            _ <- upsert (PeerPubIdent peer (entityKey key)) []
            return True

getPeerIdent :: MonadIO m => Xmpp.Jid -> PSM m (Maybe KeyID)
getPeerIdent peer = runDB $ do
    mbKey <- getBy $ UniquePeerPubIdent peer
    case mbKey of
        Nothing -> return Nothing
        Just pubIdent -> do
            mbPubIdent <- get (peerPubIdentIdent $ entityVal pubIdent)
            case mbPubIdent of
                Nothing -> return Nothing
                Just pi | Nothing <- pubIdentRevoked pi
                                -> return . Just $ pubIdentKeyID pi
                        | otherwise -> return Nothing


runDB :: MonadIO m => SqlPersistT (LoggingT (ResourceT IO)) b -> PSM m b
runDB f = do
    dbPool <- PSM $ view psDB
    PSM . liftIO . runResourceT . runStderrLoggingT . flip runSqlPool dbPool $ f

setState :: (MonadReader PSState m, MonadIO m) => PontariusState -> m ()
setState newState = do
    st <- view psState
    liftIO . atomically $ writeTVar st newState

getState :: (MonadReader PSState m, MonadIO m) => m PontariusState
getState = do
    st <- view psState
    liftIO . atomically $ readTVar st
