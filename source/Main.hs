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
import           Types
import           Xmpp

data State = State { connection :: Xmpp.Session
                   }

main = withSqlitePool "test.db3" 3 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $
        runMigration migrateAll
    xmppConRef <- newTVarIO XmppNoConnection
    propertiesRef <- newEmptyTMVarIO
    pState <- newTVarIO CredentialsUnset
    accState <- newTVarIO AccountDisabled
    let psState = PSState { _psDB = pool
                          , _psXmppCon = xmppConRef
                          , _psProps = propertiesRef
                          , _psState = pState
                          , _psAccountState = accState
                          }
        getStatus = readTVar pState
        statusProp = mkProperty pontariusObjectPath pontariusInterface
                         "Status"
                         (Just (lift $ atomically getStatus))
                         Nothing
                         PECSTrue
        ro = rootObject psState <> property statusProp
    initGPG psState
    runPSM psState updateState
    Text.writeFile "dbus-interface.xml" $ introspect "/" True ro
    con <- makeServer DBus.Session ro
    requestName "org.pontarius" def con
    manageStmProperty statusProp getStatus con
    waitFor con
