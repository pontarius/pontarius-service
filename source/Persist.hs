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
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           DBus
import           DBus.Types
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp

import           Persist.Schema
import           Types

getCredentials :: MonadIO m => PSM m (Maybe HostCredentials)
getCredentials = runDB $ fmap entityVal
                <$> selectFirst [] [Desc HostCredentialsChanged]


getCredentialsM :: PSState -> MethodHandlerT IO Text
getCredentialsM st = do
    mbCred <- runPSM st getCredentials
    case mbCred of
     Nothing -> methodError
                    $ MsgError "org.pontarius.Error.GetCredentials"
                               (Just $ "Credentials not set")
                               []
     Just cred -> return $ Text.concat [cred ^. hostnameL
                                       , "@"
                                       , cred ^. usernameL
                                       ]


setCredentials :: MonadIO m => Text -> Xmpp.Username -> Xmpp.Password -> PSM m ()
setCredentials hostName username password = do
    runDB $ do
        deleteWhere ([] :: [Filter HostCredentials])
        now <- liftIO getCurrentTime
        _ <- insert $ HostCredentials hostName username password now
        return ()
    updateState

setCredentialsM :: PSState -> Text -> Xmpp.Password -> MethodHandlerT IO ()
setCredentialsM st u p = runPSM st $ do
    case Text.splitOn "@" u of
        [h', u'] -> setCredentials h' u' p
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.SetCredentials"
                                           (Just $ "Malformed username")
                                           []
    updateState

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

setState :: (MonadReader PSState m, MonadIO m) => PontariusState -> m ()
setState newState = do
    st <- view psState
    liftIO . atomically $ writeTVar st newState

getState :: (MonadReader PSState m, MonadIO m) => m PontariusState
getState = do
    st <- view psState
    liftIO . atomically $ readTVar st


-- | Update the current state when an identifiable condition is found.
updateState :: MonadIO m => PSM m ()
updateState = do
    -- | If a condition that triggers a state transition is found we immediately
    -- return it with left.
    eNewState <- runEitherT $ do
        getCredentials `orIs` CredentialsUnset
        getSigningKey `orIs` IdentitiesAvailable
        mbCon <- liftIO . atomically . readTVar =<< view psXmppCon
        case mbCon of
            XmppNoConnection -> is Disabled
            XmppConnecting _ -> is Authenticating
            XmppConnected con -> do
                sstate <- liftIO . atomically $ Xmpp.streamState con
                case sstate of
                    Xmpp.Plain -> is Authenticating
                    Xmpp.Secured -> return ()
                    Xmpp.Closed -> is Authenticating
                    Xmpp.Finished -> return ()
    stRef <- view psState
    case eNewState of
        -- | Writing to the TVar will automatically trigger a signal if necessary
        Left newState -> do
            liftIO $ print newState
            setState newState
        -- | No state-changing condition was found, we keep the old state
        Right _ -> return ()
  where
    orIs m st = lift m >>= \case
        Just _ -> return ()
        Nothing -> left st
    is = left
