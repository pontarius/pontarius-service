{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           DBus
import           DBus.Error
import           DBus.Introspect
import           DBus.Property
import           DBus.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock
import           Data.UUID
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Xmpp (Jid)
import qualified Network.Xmpp as Xmpp

import           Basic
import           DBusInterface
import           Gpg
import           Persist
import           Transactions
import           Types
import           Xmpp

data State = State { connection :: Xmpp.Session
                   }

main :: IO ()
main = runNoLoggingT . withSqlitePool "config.db3" 3 $ \pool -> liftIO $ do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $
        runMigration migrateAll
    xmppConRef <- newTVarIO XmppNoConnection
    propertiesRef <- newEmptyTMVarIO
    pState <- newTVarIO CredentialsUnset
    accState <- newTVarIO AccountDisabled
    sem <- newEmptyTMVarIO
    let psState = PSState { _psDB = pool
                          , _psXmppCon = xmppConRef
                          , _psProps = propertiesRef
                          , _psState = pState
                          , _psAccountState = accState
                          , _psGpgCreateKeySempahore = sem
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

        ro = rootObject psState <> property statusProp
                                <> property enabledProp
                                <> property usernameProp
    con <- makeServer DBus.Session ro
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
    runPSM psState updateState
    waitFor con
