{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           DBus
import           DBus.Error
import           DBus.Property
import           DBus.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Data.UUID
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Xmpp (Jid)
import qualified Network.Xmpp as Xmpp

import           Basic
import           Persist
import           Types

data State = State { connection :: Xmpp.Session
                   }
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
                          def
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

connect = do
    sessRef <- xmppRef
    mbRef <- liftIO . atomically . tryReadTMVar $ sessRef
    case mbRef of
        Just sess -> do
            liftIO . atomically $ putTMVar sessRef sess
            return False
        Nothing -> do
            sess <- connectXmpp
            liftIO . atomically $ putTMVar sessRef sess
            return True

disconnect = do
    sessRef <- xmppRef
    ref <- liftIO . atomically . tryTakeTMVar $ sessRef
    case ref of
        Just sess -> do
            liftIO $ Xmpp.endSession sess
            return True
        Nothing -> return False

reconnect :: PSM (MethodHandlerT IO) ()
reconnect = do
    disconnect
    connect
    return ()



main = withSqlitePool "test.db3" 3 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $
        runMigration migrateAll
    xmppConRef <- newEmptyTMVarIO
    propertiesRef <- newEmptyTMVarIO
    let psState = PSState { _psDB = pool
                          , _psXmppCon = xmppConRef
                          , _psProps = propertiesRef
                          }
        conStatus = (Xmpp.streamState =<< readTMVar xmppConRef)
                    <|> (return Xmpp.Closed)
        getConStatus = do
            st <- conStatus
            return $ case st of
                Xmpp.Plain -> Connected
                Xmpp.Secured -> Connected
                Xmpp.Closed -> Disconnected
                Xmpp.Finished -> Disconnected
        setConStatus Connected = runPSM psState connect
        setConStatus Disconnected = runPSM psState disconnect
        conStatusProp = mkProperty pontariusObjectPath pontariusInterface
                                   "ConnectionStatus"
                                   (Just (atomically getConStatus))
                                   (Just setConStatus)
    con <- DBus.makeServer System undefined
    manageStmProperty conStatusProp getConStatus con
    waitFor con
