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
import           Control.Monad.Trans.Either
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

-- | Update the current state when an identifiable condition is found.
updateState :: PSState -> IO ()
updateState st = runPSM st $ do
    -- | If a condition that triggers a state transition is found we immediately
    -- return it with left.
    eNewState <- runEitherT $ do
        getCredentials `orIs` CredentialsUnset
        getSigningKey `orIs` IdentityUnset
        mbCon <- liftIO . atomically . tryReadTMVar =<< view psXmppCon
        case mbCon of
            Nothing -> is Disabled
            Just con -> do
                sstate <- liftIO . atomically $ Xmpp.streamState con
                case sstate of
                    Xmpp.Plain -> is Authenticating
                    Xmpp.Secured -> return ()
                    Xmpp.Closed -> is Authenticating
                    Xmpp.Finished -> return ()
    stRef <- view psState
    case eNewState of
        -- | Writing to the TVar will automatically trigger a signal if necessary
        Left newState -> setState newState
        -- | No state-changing condition was found, we keep the old state
        Right _ -> return ()
  where
    orIs m st = lift m >>= \case
        Just _ -> return ()
        Nothing -> left st
    is = left

main = withSqlitePool "test.db3" 3 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $
        runMigration migrateAll
    xmppConRef <- newEmptyTMVarIO
    propertiesRef <- newEmptyTMVarIO
    pState <- newTVarIO CredentialsUnset
    accState <- newTVarIO AccountDisabled
    let psState = PSState { _psDB = pool
                          , _psXmppCon = xmppConRef
                          , _psProps = propertiesRef
                          , _psState = pState
                          , _psAccountState = accState
                          }
        conStatus = (Xmpp.streamState =<< readTMVar xmppConRef)
                    <|> (return Xmpp.Closed)
        getStatus = readTVar pState
        statusProp = mkProperty pontariusObjectPath pontariusInterface
                         "Status"
                         (Just (lift $ atomically getStatus))
                         Nothing
                         PECSTrue
        ro = rootObject psState <> property statusProp
    updateState psState
    con <- DBus.makeServer DBus.Session ro
    requestName "org.pontarius" def con
    manageStmProperty statusProp getStatus con
    waitFor con
