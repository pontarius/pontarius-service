module Xmpp where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Trans
import           DBus
import           DBus.Types
import qualified Data.Text as Text

import qualified Network.Xmpp as Xmpp
import           Persist
import           Types


connectXmpp :: PSM (MethodHandlerT IO) Xmpp.Session
connectXmpp = do
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
                Left e ->
                    lift . methodError $
                        MsgError { errorName =
                                        "org.pontarius.Error.ConnectionFailure"
                                 , errorText = Just $ Text.pack (show e)
                                 , errorBody = []
                                 }
                Right r -> return r
        Nothing -> lift . methodError $
                       MsgError { errorName =
                                       "org.pontarius.Error.MissingCredentials"
                                , errorText = Just "Connection credentials are unset."
                                , errorBody = []
                                }

connect :: PSM (MethodHandlerT IO) ()
connect = do
    sessRef <- xmppRef
    mbRef <- liftIO . atomically . tryReadTMVar $ sessRef
    case mbRef of
        Just sess -> do
            liftIO . atomically $ putTMVar sessRef sess
            return ()
        Nothing -> do
            sess <- connectXmpp
            liftIO . atomically $ putTMVar sessRef sess
            return ()

disconnect :: PSM (MethodHandlerT IO) ()
disconnect = do
    sessRef <- xmppRef
    ref <- liftIO . atomically . tryTakeTMVar $ sessRef
    case ref of
        Just sess -> do
            liftIO $ Xmpp.endSession sess
            return ()
        Nothing -> return ()

reconnect :: PSM (MethodHandlerT IO) ()
reconnect = do
    _ <- disconnect
    _ <- connect
    return ()
