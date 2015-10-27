{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Transactions where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import qualified Control.Monad.Catch as Ex
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           DBus
import           DBus.Signal
import           DBus.Types (methodError)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Data.UUID (UUID)
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Persist as DB
import           Signals
import           State
import           Types
import           Xmpp as Xmpp

synchronousCreateGpgKey :: (MonadIO m, Functor m) =>
                           PSM (MethodHandlerT m) BS.ByteString
synchronousCreateGpgKey = do
    liftIO $ logDebug "Creating new identity"
    sem <- view psGpgCreateKeySempahore
    tid <- liftIO myThreadId
    liftIO (atomically $ tryPutTMVar sem tid) >>= \case
        False ->do

            lift . methodError $
                 MsgError "org.pontarius.Error.createIdentity"
                          (Just $ "Identity creation is already running")
                          []
        True -> do
            setState CreatingIdentity
            keyFpr <- liftIO newGpgKey
            now <- liftIO $ getCurrentTime
            liftIO $ logDebug "Done creating new key"
            addIdentity "gpg" (toKeyID keyFpr) (Just now) Nothing
            as <- view psAccountState
            liftIO (atomically (readTVar as)) >>= \case
                AccountEnabled -> setState Authenticating
                AccountDisabled -> setState Disabled
            void . liftIO  . atomically $ takeTMVar sem
            return keyFpr

createGpgKey :: MonadIO m => EitherT (Maybe PontariusState) (PSM m) ()
createGpgKey = do
    liftIO $ logDebug "Creating new key"
    sem <- view psGpgCreateKeySempahore
    st <- ask
    void . liftIO . forkIO . runPSM st $ do
        tid <- liftIO myThreadId
        liftIO (atomically $ tryPutTMVar sem tid) >>= \case
            False -> liftIO $ logDebug "Another thread is already creating a new key"
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

fromChallenge :: Challenge -> (UUID, Xmpp.Jid, Bool, Text, Text, Text, Bool)
fromChallenge c = ( challengeUniqueID c
                  , challengePeer c
                  , challengeOutgoing c
                  , tShow $ challengeStarted c
                  , maybe "" tShow $ challengeCompleted c
                  , fromMaybe "" (challengeQuestion c)
                  , fromMaybe False $ challengeResult c
                  )
  where
    tShow = Text.pack . show

getPeerChallengesM :: Xmpp.Jid ->
                  PSM IO [(UUID, Xmpp.Jid, Bool, Text, Text, Text, Bool)]
getPeerChallengesM peer = do
    c <- getPeerChallenges peer
    return $ map fromChallenge c

getIdentityChallengesM :: PSState -> KeyID ->
                          IO [(UUID, Xmpp.Jid, Bool, Text, Text, Text, Bool)]
getIdentityChallengesM st keyID = runPSM st $ do
    c <- getIdentityChallenges keyID
    return $ map fromChallenge c

removeChallenge :: UUID -> PSM IO ()
removeChallenge = hideChallenge

newContactM :: PSState -> Text -> IO UUID
newContactM st name = runPSM st $ newContact name

setKeyVerifiedM :: KeyID -> Bool -> PSM IO ()
setKeyVerifiedM keyID trust = setKeyVerified keyID trust

isKeyVerifiedM :: PSState -> KeyID -> MethodHandlerT IO Bool
isKeyVerifiedM st keyID = runPSM st $ do
    res <- isKeyVerified keyID
    case res of
        Nothing -> lift . methodError $
                   MsgError "org.pontarius.Error.keyTrustStatus"
                    (Just $ "No such key") []
        Just Nothing -> return False
        Just Just{} -> return True

addContactJidM :: PSState -> UUID -> Xmpp.Jid -> IO ()
addContactJidM st uuid jid = runPSM st . void $ addContactPeer uuid jid


removeContactJidM :: PSState -> Xmpp.Jid -> IO ()
removeContactJidM st = runPSM st . removeContactPeer

getAllContacts :: MonadIO m => PSM m (Map.Map UUID (Text, Set.Set a))
getAllContacts = do
    allContacts <-  getContacts
    return $ Map.fromList [ (contactUniqueID c, (contactName c, Set.empty))
                          | c <- allContacts
                          ]

removeContactM :: UUID -> PSM IO ()
removeContactM c = do
    con <- getDBusCon
    mbIds <- deleteContact c
    case mbIds of
     Nothing -> return ()
     Just ids -> liftIO $ do
        forM_ ids $ \id -> emitSignal identityUnlinkedSignal id con
        emitSignal contactRemovedSignal c con

renameContactM :: UUID -> Text -> PSM IO ()
renameContactM c name = do
    con <- getDBusCon
    renameContact c name
    liftIO $ emitSignal contactRenamedSignal (c, name) con
    return ()

addPeerM :: MonadIO m => Xmpp.Jid -> PSM (MethodHandlerT m) ()
addPeerM jid = do
    mbAdded <- Xmpp.addPeer jid
    DB.addPeer jid (isNothing mbAdded)
    return ()


addMarkedPeers :: (MonadIO m, Ex.MonadCatch m, MonadMethodError m) => PSM m ()
addMarkedPeers = do
    con <- getDBusCon
    ps <- getAddPeers
    errors <- fmap catMaybes . forM ps $ \p -> do
        let peer = (DB.peerJid p)
        res <- Ex.try $ Xmpp.addPeer peer
        case res of
         Right _ -> return Nothing
         Left (e :: MsgError) ->
             return . Just $ AddPeerFailed
               { addPeerFailedPeer = peer
               , addPeerFailedReason = fromMaybe "" $ errorText e
               }
    case errors of
     [] -> return ()
     (_:_) -> liftIO $ emitSignal addPeersFailedSignal errors con

removeMarkedPeers :: (MonadIO m, Ex.MonadCatch m, MonadMethodError m) => PSM m ()
removeMarkedPeers = do
    con <- getDBusCon
    ps <- getAddPeers
    errors <- fmap catMaybes . forM ps $ \p -> do
        let peer = (DB.peerJid p)
        res <- Ex.try $ Xmpp.removePeer peer
        case res of
         Right _ -> return Nothing
         Left (e :: MsgError) ->
             return . Just $ AddPeerFailed
               { addPeerFailedPeer = peer
               , addPeerFailedReason = fromMaybe "" $ errorText e
               }
    case errors of
     [] -> return ()
     (_:_) -> liftIO $ emitSignal addPeersFailedSignal errors con

handleRemoteRosterAvailable :: (MonadIO m, Ex.MonadCatch m, MonadMethodError m) =>
                               PSM m ()
handleRemoteRosterAvailable = do
    addMarkedPeers
    removeMarkedPeers

onXmppStateChange :: (MonadIO m, Ex.MonadCatch m, MonadMethodError m) =>
                     XmppState
                  -> PSM m ()
onXmppStateChange XmppConnected{} = handleRemoteRosterAvailable
onXmppStateChange _ = return ()
