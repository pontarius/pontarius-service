{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Xmpp where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import qualified Crypto.Hash.SHA256 as SHA256
import           DBus
import           DBus.Signal
import           DBus.Types hiding (logDebug, logError)
import           Data.ByteString (ByteString)
import           Data.Data
import           Data.Either
import           Data.Function
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.UUID (UUID)
import qualified Network.TLS as TLS
import qualified Network.Xmpp as Xmpp
import qualified Network.Xmpp.E2E as E2E
import qualified Network.Xmpp.IM as Xmpp
import           Persist
import           System.Random

import           Basic
import           Gpg
import           Signals
import           Transactions
import           Types

data XmppConnectionUpdate = NowConnected
                          | ConnectionLost
                          | ConnectionError Text
                          | AuthenticationError
                            deriving (Show, Eq, Typeable)

instance Ex.Exception XmppConnectionUpdate

tShow :: Show a => a -> Text
tShow = Text.pack . show

showJid :: Xmpp.Jid -> String
showJid = Text.unpack . Xmpp.jidToText

runHandler :: (MonadWriter [ThreadId] m, MonadIO m) => IO () -> m ()
runHandler m = do
    tid <- liftIO . forkIO $ m
    tell [tid]

-- | TODO: implement. This function is called when the connection state changed
-- (i.e. the connection has been established, was closed, could not be
-- established due to error etc.)
signalConnectionState :: XmppConnectionUpdate -> PSM IO ()
signalConnectionState _newState = return ()

xmppError :: MonadIO m => Text -> m a
xmppError = liftIO . Ex.throwIO . ConnectionError


makeE2ECallbacks :: (MonadReader PSState m, MonadIO m) =>
                    ByteString
                 -> m E2E.E2ECallbacks
makeE2ECallbacks kid = do
    st <- ask
    con <- liftIO . atomically . readTMVar =<< view psDBusConnection
    return E2E.E2EC { E2E.onStateChange =
                       \p os ns -> handleStateChange st con p os ns
                    , E2E.onSmpChallenge = \p ssid vinfo q -> do
                           -- TODO: Select Session ID
                           runPSM st $ addChallenge p ssid
                                           (toKeyID $ E2E.keyID vinfo)
                                           False q
                           let q' = maybe "" id q
                           emitSignal receivedChallengeSignal (p, q') con
                    , E2E.onSmpAuthChange = \p s -> do
                           runPSM st $ setChallengeCompleted p s
                           emitSignal peerTrustStatusChangedSignal (p, s) con
                    , E2E.cSign = signGPG kid
                    , E2E.cVerify =
                       \p pk sig txt ->
                        fmap E2E.VerifyInfo <$>
                          verifySignature st p (E2E.pubKeyData pk) sig txt

                    }

handleStateChange :: MonadIO m =>
                     PSState
                  -> DBusConnection
                  -> Xmpp.Jid
                  -> E2E.MsgState
                  -> E2E.MsgState
                  -> m ()
handleStateChange st con j oldState newState = case (oldState, newState) of
    (E2E.MsgStatePlaintext, E2E.MsgStateEncrypted vi)
        -> setOnline st con j $ toKeyID (E2E.keyID vi)
    (E2E.MsgStateFinished, E2E.MsgStateEncrypted vi)
        -> setOnline st con j $ toKeyID (E2E.keyID vi)
    (E2E.MsgStateEncrypted vi, E2E.MsgStatePlaintext)
        -> setOffline st con j $ toKeyID (E2E.keyID vi)
    (E2E.MsgStateEncrypted vi, E2E.MsgStateFinished)
        -> setOffline st con j $ toKeyID (E2E.keyID vi)
    _ -> return ()

setOnline :: MonadIO m =>
             PSState
          -> DBusConnection
          -> Xmpp.Jid
          -> KeyID
          -> m ()
setOnline st con p kid = runPSM st $ do
    mbContact <- getIdentityContact kid
    case mbContact of
        Nothing -> liftIO $
                   emitSignal unlinkedIdentityAvailabilitySignal
                              (kid, p, Available)
                              con
        Just c -> do
            (cs, _) <- liftIO $ availableContacts st
            -- | Check that the contact wasn't already online (i.e. with other
            -- peers)
            case Map.lookup c cs of
                Just ps | Set.null (p `Set.delete` ps )
                    -> liftIO $ emitSignal contactStatusChangedSignal
                                           (contactUniqueID c, Available) con
                _ -> return () -- Was already set online

setOffline :: MonadIO m =>
              PSState
           -> DBusConnection
           -> Xmpp.Jid
           -> KeyID
           -> m ()
setOffline st con p kid = runPSM st $ do
    mbContact <- getIdentityContact kid
    case mbContact of
        Nothing -> liftIO $
                   emitSignal unlinkedIdentityAvailabilitySignal
                              (kid, p, Unavailable)
                              con
        Just c -> do
            (cs, _) <- liftIO $ availableContacts st
            -- | Check that the contact wasn't already online (i.e. with other
            -- peers)
            case Map.lookup c cs of
                Just ps | Set.null (p `Set.delete` ps )
                    -> liftIO $ emitSignal contactStatusChangedSignal
                                           (contactUniqueID c, Unavailable) con
                _ -> return () -- Was already set offline or there are contacts
                               -- remaining

moveIdentity :: PSState
             -> KeyID
             -> Maybe UUID
             -> IO ()
moveIdentity st kid mbNewContact = runPSM st $ do
    con <- liftIO . atomically . readTMVar $ view psDBusConnection st
    mbContact <- fmap contactUniqueID <$> getIdentityContact kid
    unless (mbContact == mbNewContact) $ case mbNewContact of
        Just c -> do
            ps <- Map.keys <$> liftIO (sessionsByIdentity st kid)
            forM_ ps $ \p -> setOffline st con p kid
            _ <- setContactIdentity c kid
            liftIO $ emitSignal identityContactMovedSignal (kid, c) con
            forM_ ps $ \p -> setOnline st con p kid
        Nothing -> do
            ps <- Map.keys <$> liftIO (sessionsByIdentity st kid)
            forM_ ps $ \p -> setOffline st con p kid
            _ <- removeContactIdentity kid
            liftIO $ emitSignal identityUnlinkedSignal kid con
            forM_ ps $ \p -> setOnline st con p kid

-- | Make a single attempt to connect to the xmpp server
--
-- N.B.: Certain conditions may lead to a change of the current state.  The
-- change of state automatically sends an updating dbus signal and may block the
-- following connection attempts
tryConnect :: MonadIO m =>
              ByteString
           -> PSState
           -> m (Xmpp.Session, E2E.E2EContext)
tryConnect key st = runPSM st $ do
    mbCredentials <- getCredentials
    case mbCredentials of
        Just cred -> do
            let hostname = Text.unpack $ hostCredentialsHostname cred
                username = hostCredentialsUsername cred
                password = hostCredentialsPassword cred
            logDebug $ "Trying to connect to " ++ show hostname
                                        ++ " as " ++ show username
            mbKid <- getSigningKey
            kid <- case mbKid of
                Nothing -> xmppError "No signing key set"
                Just pid -> if | isJust (privIdentRevoked pid)
                                   -> xmppError "signing key is revoked"
                               | privIdentKeyBackend pid /= "gpg"
                                   -> xmppError $ "unknown key backend: "
                                        <> privIdentKeyBackend pid
                               | otherwise -> return $ privIdentKeyID pid
            cbs <- makeE2ECallbacks (fromKeyID kid)
            (e2ectx, e2eplugin) <-
                liftIO $ E2E.e2eInit (E2E.E2EG E2E.e2eDefaultParameters
                                      (E2E.PubKey "gpg" key))
                                      policy
                                      cbs
            mbSess <- liftIO $ Xmpp.session hostname
                          (Xmpp.simpleAuth username password)
                          (def & Xmpp.tlsUseNameIndicationL .~ True
                               & osc .~ (\_ _ _ _ -> return [])
                               & Xmpp.pluginsL .~ [e2eplugin]
                               & Xmpp.onPresenceChangeL .~ Just opc
                          )
            case mbSess of
                Left e -> do
                    case e of
                       (Xmpp.XmppAuthFailure _) -> do
                           setState AuthenticationDenied
                           liftIO $ Ex.throwIO AuthenticationError
                       _ -> liftIO . Ex.throwIO $  ConnectionError (tShow e)

                Right r -> do
                    _ <- liftIO $ Xmpp.sendPresence Xmpp.presenceOnline r
                    setState Authenticated
                    return (r, e2ectx)
        Nothing -> do
            setState CredentialsUnset
            liftIO . Ex.throwIO $
                ConnectionError "Connection credentials are unset."
  where
    opc peer old new = do
        case (old, new) of
             -- peers going offline should be noticed by e2e and the appropriate
             -- callback called
             (Xmpp.PeerAvailable{}, Xmpp.PeerUnavailable) -> return ()
             (Xmpp.PeerUnavailable, Xmpp.PeerAvailable{}) -> ake st peer
             _ -> return ()
    clientHooksL = lens TLS.clientHooks (\cp ch -> cp{TLS.clientHooks = ch})
    onServerCertificateL = lens TLS.onServerCertificate
                                (\ch osc-> ch {TLS.onServerCertificate = osc})
    osc = Xmpp.streamConfigurationL . Xmpp.tlsParamsL
            . clientHooksL . onServerCertificateL
    policy _peer = return $ Just True -- TODO: cross-check with DB

ake :: PSState -> Xmpp.Jid -> IO ()
ake st them = liftM (maybe () id) . runMaybeT $ do
    (xmppCon, ctx) <- (liftIO . atomically . readTVar $ view psXmppCon st)
                      >>= \case
         XmppConnected c ctx _ -> return (c, ctx)
         _ -> do
             logError "Xmpp connection not established"
             mzero
    we <- liftIO (Xmpp.getJid xmppCon) >>= \case
        Just w -> return w
        Nothing -> do
            logError "No local JID set"
            mzero
    let doStartAke = jidHash we them <= jidHash them we
    liftIO $ when doStartAke (go ctx)
  where
    jidHash j1 j2 = SHA256.hash $
                      (mappend `on` Text.encodeUtf8 . Xmpp.jidToText) j1 j2
    go ctx = do

        mbAke <- E2E.startE2E them ctx
        case mbAke of
            Right () -> return ()
            Left (E2E.AKESendError e) -> do
                logError $ "AKE with " ++ Text.unpack (Xmpp.jidToText them) ++
                    " could not be established because " ++ show e
                return ()
            Left (E2E.AKEIQError iqe) ->
                case iqe ^. errCond of
                    Xmpp.Conflict -> do
                        -- Wait for 5 to 10 seconds and try again
                        wait <- randomRIO (5, 10)
                        delay (wait * 1000000)
                        go ctx
                    -- Entity doesn't support AKE:
                    Xmpp.ServiceUnavailable -> return ()
                    -- Something went wrong
                    Xmpp.BadRequest -> do
                        logError $ "AKE with " ++ showJid them ++ " failed: "
                            ++ show (Xmpp.stanzaErrorText
                                       $ iqe ^. Xmpp.stanzaError)

                    _ -> logError $ "AKE failed: " ++ show iqe
            Left (E2E.AKEError e) -> do
                logError $ "Error during AKE: " ++ show e
                return ()


    errCond = Xmpp.stanzaError . Xmpp.stanzaErrorConditionL

-- | Try to establish a connection to the server. If the connection fails
-- another attempt is made after a (exponentially increasing) grace period. This
-- function blocks until pre-conditions are met
connector :: Integer -> PSState -> IO (Xmpp.Session, E2E.E2EContext,  [ThreadId])
connector d st = do
    let pst = st ^. psState
        as = st ^. psAccountState
        -- The following operation will only finish when all the preconditions
        -- are met _simultaneously_
    con <- liftIO . atomically $ do
        ps <- readTVar pst
        a <- readTVar as
        -- Check that we (should) have the necessary information before trying
        -- to connect
        case ps of
            Disabled -> return ()
            Authenticating -> return ()
            Authenticated -> return ()
            _ -> retry
        -- Check that the account is enabled before trying to connect
        case a of
           AccountDisabled -> retry
           AccountEnabled -> readTMVar $ st ^. psDBusConnection
    mbKey <- liftIO $ exportSigningGpgKey st
    case mbKey of
        Nothing -> (runPSM st $ setState IdentityNotFound) >> connector d st
        Just key -> do
            runPSM st $ setState Authenticating
            mbSess <- liftIO . Ex.try $ tryConnect key st
            case mbSess of
                Left e -> do
                    -- e :: XmppConnectionUpdate
                    logDebug $ "Connection failed. Waiting for " ++ show d
                           ++ " seconds"
                    runPSM st $ signalConnectionState e
                    liftIO $ delay (d * 1000000)
                        -- avoid hammering the server, delay for a certain
                        -- amount of seconds
                    connector (nextDelay d) st
                Right (sess, e2eCtx) -> do
                    sess' <- Xmpp.dupSession sess
                    (_, threads) <- runWriterT $ do
                        runHandler $ do
                            st <- Xmpp.waitForPresence ((== Xmpp.Subscribe )
                                                        . Xmpp.presenceType )
                                                          sess'
                            let Just fr = Xmpp.presenceTo st
                            emitSignal subscriptionRequestSignal fr con
                    return (sess, e2eCtx, threads)

  where
    -- Exponential back-off, we double the retry delay each time up to a total
    -- of 5 minutes and ensure that it's not below 5 seconds
    nextDelay d = max 5 (min (2*d) (5*60))

enableXmpp :: (MonadReader PSState m, MonadIO m, Functor m) => m ()
enableXmpp = do
    st <- ask
    xc <- view psXmppCon
    c <- liftIO . atomically $ readTVar xc
    case c of
        XmppConnecting _ -> return ()
        XmppConnected{} -> return ()
        XmppNoConnection -> void . liftIO .  forkIO $ do
            tid <- myThreadId
            -- | To avoid a race condition where multiple connection threads are
            -- started we try to write our own ThreadId into the ref. If the ref
            -- already contains something else we lost the race and abort
            run <- atomically $ do
                x <- readTVar xc
                case x of
                    XmppNoConnection -> do
                        writeTVar xc (XmppConnecting tid)
                        return True
                    _ -> return False
            case run of
                 False -> return ()
                 True -> do
                     (sess, e2eCtx, threads) <- connector 0 st
                     atomically . writeTVar xc $ XmppConnected sess e2eCtx threads

enableAccount :: (Functor m, MonadIO m) => PSM (MethodHandlerT m) ()
enableAccount = do
    as <- view psAccountState
    liftIO . atomically $ writeTVar as AccountEnabled
    enableXmpp

disconnect :: Xmpp.Session -> IO ()
disconnect con = do
    _ <- Xmpp.sendPresence Xmpp.presenceOffline con
    Xmpp.endSession con

disableAccount :: (MonadReader PSState m, MonadIO m) => m ()
disableAccount = do
    as <- view psAccountState
    xmppRef <- view psXmppCon
    mbXmppSess  <- liftIO . atomically $ do
        writeTVar as AccountDisabled
        readTVar xmppRef
    case mbXmppSess of
        XmppConnecting tid -> liftIO $ killThread tid
        XmppConnected con _ threads -> do
            liftIO $ disconnect con
            liftIO . forM_ threads $ killThread
        XmppNoConnection -> return ()
    liftIO  . atomically $ writeTVar xmppRef XmppNoConnection


getSession :: PSM (MethodHandlerT IO) Xmpp.Session
getSession = do
    xc <- view psXmppCon
    c <- liftIO $ readTVarIO xc
    case c of
        XmppConnected sess _ _ -> return sess
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.Xmpp"
                                      (Just $ "Not connected")
                                      []
getE2EContext :: PSM (MethodHandlerT IO) E2E.E2EContext
getE2EContext = do
    xc <- view psXmppCon
    c <- liftIO $ readTVarIO xc
    case c of
        XmppConnected _ ctx _ -> return ctx
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.Xmpp"
                                      (Just $ "Not connected")
                                      []

getE2EContextSTM :: PSState -> STM E2E.E2EContext
getE2EContextSTM st = do
    c <- readTVar (view psXmppCon st)
    case c of
        XmppConnected _ ctx _ -> return ctx
        _ -> retry


getSessionSTM :: PSState -> STM Xmpp.Session
getSessionSTM st = do
    c <- readTVar (st ^. psXmppCon)
    case c of
        XmppConnected sess _ _ -> return sess
        _ -> retry


getXmppRoster :: PSM (MethodHandlerT IO) (Map Xmpp.Jid Xmpp.Item)
getXmppRoster = do
    sess <- getSession
    Xmpp.items <$> liftIO (Xmpp.getRoster sess)

getPeersSTM :: PSState -> STM [(Xmpp.Jid, Bool)]
getPeersSTM st = do
    sess <- getSessionSTM st
    peers <- map Xmpp.riJid . filter ((== Xmpp.Both) . Xmpp.riSubscription)
                 . Map.elems . Xmpp.items <$> Xmpp.getRosterSTM sess
    forM peers $ \peer -> do
        av <- Xmpp.isPeerAvailable peer sess
        return (peer, av)
  <|> return []

subscribe :: Xmpp.Jid -> PSM (MethodHandlerT IO) (Either Xmpp.XmppFailure ())
subscribe peer = do
    sess <- getSession
    liftIO $ Xmpp.sendPresence (Xmpp.presenceSubscribe peer) sess

acceptSubscription :: Xmpp.Jid -> PSM (MethodHandlerT IO) (Either Xmpp.XmppFailure ())
acceptSubscription peer = do
    sess <- getSession
    liftIO $ Xmpp.sendPresence (Xmpp.presenceSubscribed peer) sess

addPeer :: Xmpp.Jid -> PSM (MethodHandlerT IO) ()
addPeer peer = do
    -- TODO: be more precise here. We have to check that the server supports
    -- pre-approval
    sess <- getSession
    _ <- liftIO $ Xmpp.sendPresence (Xmpp.presenceSubscribe peer) sess
    _ <- liftIO $ Xmpp.sendPresence (Xmpp.presenceSubscribed peer) sess
    return ()

removePeer :: Xmpp.Jid -> PSM (MethodHandlerT IO) ()
removePeer peer = do
    sess <- getSession
    _ <- liftIO $ Xmpp.sendPresence (Xmpp.presenceUnsubscribed peer) sess
    _ <- liftIO $ Xmpp.sendPresence (Xmpp.presenceUnsubscribe peer) sess
    return ()

getAvailableXmppPeers :: PSM (MethodHandlerT IO) [Xmpp.Jid]
getAvailableXmppPeers = do
    sess <- getSession
    liftIO . atomically $ do
        ps <- Xmpp.getAvailablePeers sess
        fmap concat . forM ps $ \p -> Map.keys <$> Xmpp.getPeerEntities p sess

getAvailableXmppPeersSTM :: PSState -> STM [Xmpp.Jid]
getAvailableXmppPeersSTM st = do
    sess <- getSessionSTM st
    Xmpp.getAvailablePeers sess

startAKE :: Xmpp.Jid -> PSM (MethodHandlerT IO) (Either E2E.AKEError ())
startAKE peer = do
    e2eCtx <- getE2EContext
    liftIO $ E2E.startE2E peer e2eCtx

verifyChannel :: Xmpp.Jid
              -> Text
              -> Text
              -> PSM (MethodHandlerT IO) ()
verifyChannel peer question secret = do
    ctx <- getE2EContext
    let mbQuestion = if Text.null question then Nothing else Just question
    res <- liftIO $ E2E.startSmp peer mbQuestion secret ctx
    case res of
        Left e -> lift . methodError $ MsgError "pontarius.Xmpp.SMP"
                                         (Just $ tShow e) []
        Right (ssid, vinfo) ->
            addChallenge peer ssid (toKeyID $ E2E.keyID vinfo) True mbQuestion

respondChallenge :: Xmpp.Jid -> Text -> PSM (MethodHandlerT IO) ()
respondChallenge peer secret = do
    ctx <- getE2EContext
    res <- E2E.answerChallenge peer secret ctx
    case res of
     Left e -> lift . methodError $ MsgError "pontarius.Xmpp.SMP"
                                         (Just $ tShow e) []
     Right r -> return r

getSessions :: PSState -> IO (Map Xmpp.Jid E2E.SessionState)
getSessions st = atomically $ do
    c <- readTVar (view psXmppCon st)
    case c of
        XmppConnected _ ctx _ -> E2E.getSessions ctx
        _ -> return Map.empty


sessionsByIdentity :: PSState -> KeyID -> IO (Map Xmpp.Jid E2E.SessionState)
sessionsByIdentity st kid = do
    sess <- getSessions st
    return $ Map.filter hasKid sess
  where
    hasKid E2E.Authenticated{E2E.sessionVerifyInfo = vinfo}
        = toKeyID (E2E.keyID vinfo) == kid
    hasKid _ = False

availableContacts :: PSState -> IO (Map Contact (Set Xmpp.Jid), [Xmpp.Jid])
availableContacts st = do
    sessions <- getSessions st
    -- "online" (i.e. authenticated) peers
    let ops = mapMaybe isAuthenticated $ Map.toList sessions
    (cs, noCs) <- fmap partitionEithers . forM ops $ \(p, pkId) -> do
        mbC <- runPSM st $ getIdentityContact (toKeyID pkId)
        case mbC of
            Nothing -> return $ Right p
            Just c -> return $ Left $ Map.singleton c (Set.singleton p)
    return (List.foldl' (Map.unionWith Set.union) Map.empty cs, noCs)
  where
    isAuthenticated (p, E2E.Authenticated{E2E.sessionVerifyInfo = vi})
           = Just (p, E2E.keyID vi)
    isAuthenticated _ = Nothing
