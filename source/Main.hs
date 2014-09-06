{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           DBus
import           DBus.Introspect (introspect)
import           DBus.Property
import           Data.Monoid
import qualified Data.Text.IO as Text
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp
import           System.Exit
import           System.Log.Logger

import           Basic
import           DBusInterface
import           Persist
import           Transactions
import           Types
import           Xmpp

data State = State { connection :: Xmpp.Session
                   }

main :: IO ()
main = runStderrLoggingT . withSqlitePool "config.db3" 3 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    xmppConRef <- newTVarIO XmppNoConnection
    propertiesRef <- newEmptyTMVarIO
    pState <- newTVarIO CredentialsUnset
    accState <- newTVarIO AccountDisabled
    sem <- newEmptyTMVarIO
    conRef <- newEmptyTMVarIO
    let psState = PSState { _psDB = pool
                          , _psXmppCon = xmppConRef
                          , _psProps = propertiesRef
                          , _psState = pState
                          , _psAccountState = accState
                          , _psGpgCreateKeySempahore = sem
                          , _psDBusConnection = conRef
                          }
        getStatus = readTVar pState
        getEnabled = (== AccountEnabled) <$> readTVar accState
        statusProp = mkProperty pontariusObjectPath pontariusInterface
                         "Status"
                         (Just (lift $ atomically getStatus))
                         Nothing
                         PECSTrue
        enabledProp = mkProperty pontariusObjectPath pontariusInterface
                         "AccountEnabled"
                         (Just (lift . atomically $ getEnabled))
                         (Just $ \e -> do
                           newState <- case e of
                                True -> (runPSM psState $ enableAccount)
                                          >> return AccountEnabled

                                False -> runPSM psState $ disableAccount
                                          >> return AccountDisabled
                           liftIO . atomically $ writeTVar accState newState
                           return True
                         )
                         PECSTrue
        usernameProp = mkProperty  pontariusObjectPath pontariusInterface
                         "Username"
                         (Just (getCredentialsM psState))
                         Nothing
                         PECSFalse
        peersProp = mkProperty  pontariusObjectPath pontariusInterface
                         "Peers"
                         (Just $ runPSM psState getPeers)
                         Nothing
                         PECSFalse -- TODO
        availableEntitiesProp = mkProperty pontariusObjectPath pontariusInterface
                         "AvailableEntities"
                         (Just $ runPSM psState getAvailableXmppPeers)
                         Nothing
                         PECSTrue
        ro = rootObject psState <> property statusProp
                                <> property enabledProp
                                <> property usernameProp
                                <> property peersProp
                                <> property availableEntitiesProp
    con <- makeServer DBus.Session ro
    atomically $ putTMVar conRef con
    requestName "org.pontarius" def con >>= liftIO . \case
        PrimaryOwner -> return ()
        DBus.InQueue -> do
            debug "dbus name is already taken"
            exitSuccess
        DBus.Exists -> do
            debug "dbus name is already taken"
            exitSuccess
        DBus.AlreadyOwner -> do
            debug "dbus server reports \"already owner\"?!?"
    manageStmProperty statusProp getStatus con
    manageStmProperty enabledProp  getEnabled con
    manageStmProperty availableEntitiesProp (getAvailableXmppPeersSTM psState) con
    debug "updateing state"
    runPSM psState updateState
    debug "done updating state"

    waitFor con
