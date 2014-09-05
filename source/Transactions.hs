{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Transactions where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           DBus.Types
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Persist
import           Types

-- | Update the current state when an identifiable condition is found.
updateState :: MonadIO m => PSM m ()
updateState = do
    -- | If a condition that triggers a state transition is found we immediately
    -- return it with left.
    eNewState <- runEitherT $ do
        liftIO . debug . show =<< lift getCredentials
        getCredentials `orIs` (Just CredentialsUnset)
        lift getSigningKey  >>= \case
            Nothing -> liftIO getIdentities >>= \case
                [] -> do createGpgKey
                         is Nothing
                _ -> is (Just IdentitiesAvailable)
            Just key -> do
                ids <- liftIO getIdentities
                if privIdentKeyID key `elem` ids
                   then return ()
                   else is (Just IdentityNotFound)
        mbCon <- liftIO . atomically . readTVar =<< view psXmppCon
        case mbCon of
            XmppNoConnection -> is (Just Disabled)
            XmppConnecting _ -> is (Just Authenticating)
            XmppConnected con _ -> do
                sstate <- liftIO . atomically $ Xmpp.streamState con
                case sstate of
                    Xmpp.Plain -> is (Just Authenticating)
                    Xmpp.Secured -> is Nothing
                    Xmpp.Closed -> is (Just Authenticating)
                    Xmpp.Finished -> is Nothing
    case eNewState of
        -- | Writing to the TVar will automatically trigger a signal if necessary
        Left (Just newState) -> do
            liftIO $ print newState
            setState newState
        -- | No state-changing condition was found, we keep the old state
        _ -> return ()
  where
    orIs m st = lift m >>= \case
        Just _ -> return ()
        Nothing -> left st
    is = left

synchronousCreateGpgKey :: (MonadIO m, Functor m) =>
                          PSM (MethodHandlerT m) BS.ByteString
synchronousCreateGpgKey = do
    liftIO $ debug "Creating new identity"
    sem <- view psGpgCreateKeySempahore
    tid <- liftIO myThreadId
    liftIO (atomically $ tryPutTMVar sem tid) >>= \case
        False -> lift . methodError $
                 MsgError "org.pontarius.Error.createIdentity"
                          (Just $ "Identity creation is already running")
                          []
        True -> do
            setState CreatingIdentity
            keyFpr <- liftIO newGpgKey
            now <- liftIO $ getCurrentTime
            liftIO $ debug "Done creating new key"
            addIdentity "gpg" keyFpr (Just now) Nothing
            as <- view psAccountState
            liftIO (atomically (readTVar as)) >>= \case
                AccountEnabled -> setState Authenticating
                AccountDisabled -> setState Disabled
            void . liftIO  . atomically $ takeTMVar sem
            return keyFpr

createGpgKey :: MonadIO m => EitherT (Maybe PontariusState) (PSM m) ()
createGpgKey = do
    liftIO $ debug "Creating new key"
    sem <- view psGpgCreateKeySempahore
    st <- ask
    void . liftIO . forkIO . runPSM st $ do
        tid <- liftIO myThreadId
        liftIO (atomically $ tryPutTMVar sem tid) >>= \case
            False -> liftIO $ debug "Another thread is already creating a new key"
            True -> do

                return ()
    left Nothing

setCredentialsM :: PSState -> Text -> Xmpp.Password -> MethodHandlerT IO ()
setCredentialsM st u p = runPSM st $ do
    case Text.splitOn "@" u of
        [u', h'] -> do
            setCredentials h' u' p
            st <- liftIO $ runPSM st getState
            case st of
                CredentialsUnset -> updateState
                _ -> return ()
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.SetCredentials"
                                           (Just $ "Malformed username")
                                           []

getCredentialsM :: PSState -> MethodHandlerT IO Text
getCredentialsM st = do
    mbCred <- runPSM st getCredentials
    case mbCred of
     Nothing -> methodError
                    $ MsgError "org.pontarius.Error.GetCredentials"
                               (Just $ "Credentials not set")
                               []
     Just cred -> return $ Text.concat [cred ^. usernameL
                                       , "@"
                                       , cred ^. hostnameL
                                       ]

setSigningGpgKeyM :: PSState
                  -> KeyID
                  -> MethodHandlerT IO ()
setSigningGpgKeyM st keyFpr = do
    haveKey <- liftIO $ setSigningGpgKey st keyFpr
    case haveKey of
        True -> lift $ runPSM st updateState
        False -> methodError $
                 MsgError { errorName = "org.pontarius.Error.setIdentity"
                          , errorText = Just "No such identity"
                          , errorBody = []
                          }
