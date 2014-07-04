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
import           Data.Monoid
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
import           DBusInterface
import           Gpg
import           Persist
import           Types

data State = State { connection :: Xmpp.Session
                   }

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
        conStatusProp = mkProperty pontariusObjectPath pontariusInterface
                                   "ConnectionStatus"
                                   (Just (lift $ atomically getConStatus))
                                   Nothing
                                   PECSTrue
        ro = rootObject psState <> property conStatusProp
    con <- DBus.makeServer DBus.Session ro
    requestName "pontarius.service" def con
    manageStmProperty conStatusProp getConStatus con
    waitFor con
