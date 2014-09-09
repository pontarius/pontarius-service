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
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           DBus
import           DBus.Signal
import           DBus.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Data
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.XML.Pickle
import           Data.XML.Types
import qualified Network.TLS as TLS
import qualified Network.Xmpp as Xmpp
import qualified Network.Xmpp.E2E as E2E
import qualified Network.Xmpp.IM as Xmpp
import           Persist

import           Basic
import           Gpg
import           Types

data XmppConnectionUpdate = NowConnected
                          | ConnectionLost
                          | ConnectionError Text
                          | AuthenticationError
                            deriving (Show, Eq, Typeable)

instance Ex.Exception XmppConnectionUpdate

tShow :: Show a => a -> Text
tShow = Text.pack . show

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
    let trustStatusSignal peer status
            = Signal { signalPath = pontariusObjectPath
                     , signalInterface = pontariusInterface
                     , signalMember = "peerTrustStatusChanged"
                     , signalBody = [ DBV $ toRep peer
                                    , DBV . toRep $ tShow status
                                    ]
                     }
        peerStatusSignal peer status
            = Signal { signalPath = pontariusObjectPath
                     , signalInterface = pontariusInterface
                     , signalMember = "peerStatusChanged"
                     , signalBody = [ DBV $ toRep peer
                                    , DBV . toRep $ tShow status
                                    ]
                     }
        receivedChallengeSignal peer mbQuestion
            = Signal { signalPath = pontariusObjectPath
                     , signalInterface = pontariusInterface
                     , signalMember = "receivedChallenge"
                     , signalBody = [ DBV $ toRep peer
                                    , DBV . toRep $ fromMaybe "" mbQuestion
                                    ]
                     }

    return E2E.E2EC { E2E.onSendMessage = \_ _ -> return ()
                    , E2E.onStateChange =
                       \p s -> emitSignal (peerStatusSignal p s ) con
                    , E2E.onSmpChallenge = \p q -> do
                           -- TODO: Select Session ID
                           runPSM st $ addChallenge p "todo" False q
                           emitSignal (receivedChallengeSignal p q) con
                    , E2E.onSmpAuthChange = \p s -> do
                           runPSM st $ setChallengeCompleted p s
                           emitSignal (trustStatusSignal p s) con
                    , E2E.cSign = signGPG kid
                    , E2E.cVerify =
                       \p pk sig txt -> verifyGPG p (E2E.pubKeyIdent pk) sig txt

                    }

-- | Make a single attempt to connect to the xmpp server
--
-- N.B.: Certain conditions may lead to a change of the current state.  The
-- change of state automatically sends an updating dbus signal and may block the
-- following connection attempts
tryConnect :: MonadIO m =>
              PSState
           -> m (Xmpp.Session, E2E.E2EContext)
tryConnect st = runPSM st $ do
    mbCredentials <- getCredentials
    case mbCredentials of
        Just cred -> do
            let hostname = Text.unpack $ hostCredentialsHostname cred
                username = hostCredentialsUsername cred
                password = hostCredentialsPassword cred
            debug $ "Trying to connect to " ++ show hostname
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
                                      (E2E.PubKey "gpg" (fromKeyID kid)))
                                      policy
                                      cbs
            mbSess <- liftIO $ Xmpp.session hostname
                          (Xmpp.simpleAuth username password)
                          (def & Xmpp.tlsUseNameIndicationL .~ True
                               & osc .~ (\_ _ _ _ -> return [])
                               & Xmpp.pluginsL .~ [e2eplugin]
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
    clientHooksL = lens TLS.clientHooks (\cp ch -> cp{TLS.clientHooks = ch})
    onServerCertificateL = lens TLS.onServerCertificate
                                (\ch osc-> ch {TLS.onServerCertificate = osc})
    osc = Xmpp.streamConfigurationL . Xmpp.tlsParamsL
            . clientHooksL . onServerCertificateL
    policy peer = runPSM st $ Just <$> checkPeerIdent peer

-- | Check the local store for peer's key, if there is none, attempt to retrieve
-- it
--
-- Returns True if a key could be found
checkPeerIdent :: (MonadIO m, Functor m) => Xmpp.Jid -> PSM m Bool
checkPeerIdent peer= do
    let barePeer = Xmpp.toBare peer
    mbKey <- getPeerIdent barePeer
    case mbKey of
        Just key -> do
            debug $ "found key " ++ show key ++ " for peer " ++ show barePeer
            return True
        Nothing -> do
            mbKey <- requestPubkey peer
            case mbKey of
                Nothing -> do
                    debug $ "Could not acquire key for peer " ++ show peer
                    return False
                Just key -> do
                    debug $ "Got key " ++ show key ++ " from peer " ++ show peer
                    addPubIdent (toKeyID key)
                    _ <- associatePubIdent barePeer (toKeyID key)
                    return $ True


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
    runPSM st $ setState Authenticating
    mbSess <- liftIO . Ex.try $ tryConnect st
    case mbSess of
        Left e -> do
            -- e :: XmppConnectionUpdate
            debug $ "Connection failed. Waiting for " ++ show d ++ " seconds"
            runPSM st $ signalConnectionState e
            liftIO $ delay (d * 1000000)
                -- avoid hammering the server, delay for a certain amount of
                -- seconds
            connector (nextDelay d) st
        Right (sess, e2eCtx) -> do
            sess' <- Xmpp.dupSession sess
            (_, threads) <- runWriterT $ do
                runHandler $ do
                    st <- Xmpp.waitForPresence ((== Xmpp.Subscribe )
                                                . Xmpp.presenceType ) sess'
                    let Just fr = Xmpp.presenceTo st
                        sig = Signal { signalPath = pontariusObjectPath
                                     , signalInterface = pontariusInterface
                                     , signalMember = "subscriptionRequest"
                                     , signalBody = [DBV $ toRep fr]
                                     }
                    emitSignal sig con
                runHandler $ Xmpp.runIQHandler (handlePubkeyRequest st) sess
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
    c <- liftIO . atomically $ readTVar xc
    case c of
        XmppConnected sess _ _ -> return sess
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.Xmpp"
                                      (Just $ "Not connected")
                                      []
getE2EContext :: PSM (MethodHandlerT IO) E2E.E2EContext
getE2EContext = do
    xc <- view psXmppCon
    c <- liftIO . atomically $ readTVar xc
    case c of
        XmppConnected _ ctx _ -> return ctx
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.Xmpp"
                                      (Just $ "Not connected")
                                      []

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

getPeers :: PSM (MethodHandlerT IO) [(Xmpp.Jid, Bool)]
getPeers = do
    sess <- getSession
    jids <- map Xmpp.riJid . filter ((== Xmpp.Both) . Xmpp.riSubscription)
           . Map.elems <$> getXmppRoster
    liftIO . atomically $ forM jids $ \j -> (j,) <$> Xmpp.isPeerAvailable j sess


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

startAKE :: Xmpp.Jid -> PSM (MethodHandlerT IO) Bool
startAKE peer = do
    haveKey <- checkPeerIdent peer
    case haveKey of
        False -> return False
        True -> do
            e2eCtx <- getE2EContext
            liftIO $ E2E.startE2E peer e2eCtx

data PubkeyQuery = PubkeyQuery deriving Show

xpPubkeyQuery :: PU [Node] ()
xpPubkeyQuery = xpElemBlank "{yabasta-pubkey-1:0}pubkey-query"

xpBase64 :: PU t Text -> PU t ByteString
xpBase64 = xpWrap B64.decodeLenient B64.encode
             . xpWrap Text.encodeUtf8 Text.decodeUtf8

instance Xmpp.IQRequestClass PubkeyQuery where
    data IQResponseType PubkeyQuery =
        PubkeyResponse {fromPubkeyResponse :: BS.ByteString}
    pickleRequest  = xpRoot . xpUnliftElems . xpConst PubkeyQuery
                       $ xpElemBlank "{yabasta-pubkey-1:0}pubkey-query"
    pickleResponse = xpUnliftElems .
                     xpElemNodes "{yabasta-pubkey-1:0}pubkey-response" .
                       xpWrap PubkeyResponse fromPubkeyResponse .
                         xpBase64 $ xpContent xpId
    requestType _ = Xmpp.Get
    requestNamespace _ = "yabasta-pubkey-1:0"

handlePubkeyRequest :: PSState -> Xmpp.IQRequestHandler PubkeyQuery
handlePubkeyRequest st _req = do
    mbKey <- exportSigningGpgKey st
    case mbKey of
        Nothing -> return . Left $ Xmpp.mkStanzaError Xmpp.ItemNotFound
        Just key -> return . Right $ PubkeyResponse key

requestPubkey :: (MonadIO m, Functor m) =>
                 Xmpp.Jid
              -> PSM m (Maybe BS.ByteString)
requestPubkey peer = do
    st <- PSM ask
    mbSess <- liftIO . atomically $ Just <$> getSessionSTM st
                                    <|> return Nothing
    case mbSess of
     Nothing -> do
         debug "requestPubkey: not connected"
         return Nothing
     Just sess -> do
         resp <- liftIO . runExceptT $
                     Xmpp.sendIQRequest (Just 10000000)
                                        (Just peer) PubkeyQuery sess
         case resp of
             Left e -> do
                 debug $ "requestPubkey returned error: " ++ show e
                 return Nothing
             Right (Left e) -> do
                 debug $ "requestPubkey returned error: " ++ show e
                 return Nothing
             Right (Right r) -> listToMaybe <$>
                                    importKey peer (fromPubkeyResponse r)

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
        Right _ -> addChallenge peer "todo" True mbQuestion

respondChallenge :: Xmpp.Jid -> Text -> PSM (MethodHandlerT IO) ()
respondChallenge peer secret = do
    ctx <- getE2EContext
    res <- E2E.answerChallenge peer secret ctx
    case res of
     Left e -> lift . methodError $ MsgError "pontarius.Xmpp.SMP"
                                         (Just $ tShow e) []
     Right r -> return r
