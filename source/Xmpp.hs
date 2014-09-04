{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Xmpp where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay
import           Control.Concurrent.Timeout
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans
import           DBus
import           DBus.Types
import           Data.Data
import qualified Data.Foldable as Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable

import qualified Network.Xmpp as Xmpp
import           Persist
import           Types

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
signalConnectionState newState = return ()

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
            let hostname = hostCredentialsHostname cred
                username = hostCredentialsUsername cred
                password = hostCredentialsPassword cred
            mbSess <- liftIO $ Xmpp.session (Text.unpack hostname)
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
connector :: Integer -> PSState -> IO Xmpp.Session
connector d st = do
    let pst = st ^. psState
        as = st ^. psAccountState
        xmppRef =  st ^. psXmppCon
        -- The following operation will only finish when all the preconditions
        -- are met _simultaneously_
    mbXmppCon <- liftIO . atomically $ do
        ps <- readTVar pst
        a <- readTVar as
        -- Check that we (should) have the necessary information before trying
        -- to connect
        case ps of
            CredentialsUnset -> retry
            IdentityUnset -> retry
            AuthenticationDenied -> retry
            _ -> return ()
        -- Check that the account is enabled before trying to connect
        case a of
           AccountDisabled -> retry
           AccountEnabled -> return ()
    mbSess <- liftIO . Ex.try $ tryConnect st
    case mbSess of
        Left e -> do
            -- e :: XmppConnectionUpdate
            runPSM st $ signalConnectionState e
            liftIO $ delay (d * 10^9)
                -- avoid hammering the server, delay for a certain amount of
                -- seconds
            connector (nextDelay d) st
        Right sess -> return sess
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
        XmppConnected _ -> return ()
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
                     sess <- connector 0 st
                     atomically . writeTVar xc $ XmppConnected sess

enableAccount :: (Functor m, MonadIO m) => PSM (MethodHandlerT m) ()
enableAccount = do
    as <- view psAccountState
    liftIO . atomically $ writeTVar as AccountEnabled
    enableXmpp

disconnect :: Xmpp.Session -> IO ()
disconnect con = do
    Xmpp.sendPresence Xmpp.presenceOffline con
    Xmpp.endSession con

disableAccount = do
    as <- view psAccountState
    xmppRef <- view psXmppCon
    mbXmppSess  <- liftIO . atomically $ do
        writeTVar as AccountDisabled
        readTVar xmppRef
    case mbXmppSess of
        XmppConnecting tid -> liftIO $ killThread tid
        XmppConnected con -> liftIO $ disconnect con
        XmppNoConnection -> return ()

-- connect :: PSM (MethodHandlerT IO) ()
-- connect = do
--     sessRef <- xmppRef
--     mbRef <- liftIO . atomically . tryReadTMVar $ sessRef
--     case mbRef of
--         Just sess -> do
--             liftIO . atomically $ putTMVar sessRef sess
--             return ()
--         Nothing -> do
--             sess <- connectXmpp
--             liftIO . atomically $ putTMVar sessRef sess
--             return ()

-- disconnect :: PSM (MethodHandlerT IO) ()
-- disconnect = do
--     sessRef <- xmppRef
--     ref <- liftIO . atomically . tryTakeTMVar $ sessRef
--     case ref of
--         Just sess -> do
--             liftIO $ Xmpp.endSession sess
--             return ()
--         Nothing -> return ()

-- reconnect :: PSM (MethodHandlerT IO) ()
-- reconnect = do
--     _ <- disconnect
--     _ <- connect
--     return ()
