{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Transactions where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Trans
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
        getCredentials `orIs` CredentialsUnset
        lift getSigningKey  >>= \case
            Nothing -> liftIO getIdentities >>= \case
                [] -> do _ <- lift createGpgKey
                         as <- lift $ view psAccountState
                         liftIO (atomically (readTVar as)) >>= \case
                             AccountEnabled -> is Authenticating
                             AccountDisabled -> is Disabled
                _ -> is IdentitiesAvailable
            Just key -> do
                ids <- liftIO getIdentities
                if privIdentKeyID key `elem` ids
                   then return ()
                   else is IdentityNotFound
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

createGpgKey :: MonadIO m => PSM m BS.ByteString
createGpgKey = do
    liftIO $ debug "Creating new key"
    s <- getState
    case s of
     CreatingIdentity -> error "Already in state CreatingIdentity"
     _ -> return ()
    setState CreatingIdentity
    keyFpr <- liftIO newGpgKey
    now <- liftIO $ getCurrentTime
    liftIO $ debug "Done creating new key"
    addIdentity "gpg" keyFpr (Just now) Nothing
    return keyFpr

setCredentialsM :: PSState -> Text -> Xmpp.Password -> MethodHandlerT IO ()
setCredentialsM st u p = runPSM st $ do
    case Text.splitOn "@" u of
        [h', u'] -> do
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
     Just cred -> return $ Text.concat [cred ^. hostnameL
                                       , "@"
                                       , cred ^. usernameL
                                       ]
