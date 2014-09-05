{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Xmpp where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.Reader
import           DBus
import           DBus.Signal
import           DBus.Types
import           Data.Data
import           Data.Map (Map)

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text


import qualified Network.Xmpp as Xmpp
import qualified Network.Xmpp.IM as Xmpp
import           Persist
import           Types
import           Basic

data XmppConnectionUpdate = NowConnected
                          | ConnectionLost
                          | ConnectionError Text
                          | AuthenticationError
                            deriving (Show, Eq, Typeable)

instance Ex.Exception XmppConnectionUpdate

tShow :: Show a => a -> Text
tShow = Text.pack . show

-- | TODO: implement. This function is called when the connection state changed
-- (i.e. the connection has been established, was closed, could not be
-- established due to error etc.)
signalConnectionState :: XmppConnectionUpdate -> PSM IO ()
signalConnectionState _newState = return ()

-- | Make a single attempt to connect to the xmpp server
--
-- N.B.: Certain conditions may lead to a change of the current state.  The
-- change of state automatically sends an updating dbus signal and may block the
-- following connection attempts
tryConnect :: MonadIO m => PSState -> m Xmpp.Session
tryConnect st = runPSM st $ do
    mbCredentials <- getCredentials
    case mbCredentials of
        Just cred -> do
            let hostname = Text.unpack $ hostCredentialsHostname cred
                username = hostCredentialsUsername cred
                password = hostCredentialsPassword cred
            debug $ "Trying to connect to " ++ show hostname
                                        ++ " as " ++ show username
            mbSess <- liftIO $ Xmpp.session hostname
                          (Xmpp.simpleAuth username password)
                          (def & Xmpp.tlsUseNameIndicationL .~ False)
            case mbSess of
                Left e -> do
                    case e of
                       (Xmpp.XmppAuthFailure _) -> do
                           setState AuthenticationDenied
                           liftIO $ Ex.throwIO AuthenticationError
                       _ -> liftIO . Ex.throwIO $  ConnectionError (tShow e)

                Right r -> do
                    setState Authenticated
                    return r
        Nothing -> do
            setState CredentialsUnset
            liftIO . Ex.throwIO $
                ConnectionError "Connection credentials are unset."

-- | Try to establish a connection to the server. If the connection fails
-- another attempt is made after a (exponentially increasing) grace period. This
-- function blocks until pre-conditions are met
connector :: Integer -> PSState -> IO (Xmpp.Session, [ThreadId])
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
        Right sess -> do
            sess' <- Xmpp.dupSession sess
            subscriptions <- forkIO $ do
                st <- Xmpp.waitForPresence ((== Xmpp.Subscribe )
                                            . Xmpp.presenceType ) sess'
                let Just fr = Xmpp.presenceTo st
                    sig = Signal { signalPath = pontariusObjectPath
                                 , signalInterface = pontariusInterface
                                 , signalMember = "subscriptionRequest"
                                 , signalBody = [DBV $ toRep fr]
                                 }
                emitSignal sig con
            return (sess, [subscriptions])

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
        XmppConnected _ _ -> return ()
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
                     (sess, threads) <- connector 0 st
                     atomically . writeTVar xc $ XmppConnected sess threads

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
        XmppConnected con threads -> do
            liftIO $ disconnect con
            liftIO . forM_ threads $ killThread
        XmppNoConnection -> return ()
    liftIO  . atomically $ writeTVar xmppRef XmppNoConnection


getXmppRoster :: PSM (MethodHandlerT IO) (Map Xmpp.Jid Xmpp.Item)
getXmppRoster = do
    xc <- view psXmppCon
    c <- liftIO . atomically $ readTVar xc
    case c of
        XmppConnected sess _ -> Xmpp.items <$> liftIO (Xmpp.getRoster sess)
        _ -> lift $ methodError $ MsgError "org.pontarius.Error.getRoster"
                                      (Just $ "Not connected")
                                      []

getPeers :: PSM (MethodHandlerT IO) [Xmpp.Jid]
getPeers = map Xmpp.riJid . filter Xmpp.riApproved . Map.elems <$> getXmppRoster
