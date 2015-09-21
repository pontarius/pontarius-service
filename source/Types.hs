{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Types
  ( module PontariusService.Types
  , module Types
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           DBus
import           DBus.Signal
import           DBus.Types
import           Data.Set (Set)
import qualified Data.Set as Set
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp
import qualified Network.Xmpp.E2E as Xmpp

import           PontariusService.Types



class Signaling m where
    signals :: SomeSignal -> m ()

instance Signaling (MethodHandlerT IO) where
    signals = signal'

instance (MonadTrans t, MonadReader DBusConnection (t IO)) => Signaling (t IO) where
    signals s = do
        dbc <- ask
        lift $ emitSignal' s dbc

data PSProperties = PSProperties{ _pspConnectionStatus :: Property (RepType Bool)}

-- | When requesting a new connection we fork a new thread that creates the
-- connection and sets it's own threadID so it can be aborted when required
data XmppState = XmppConnecting ThreadId
               | XmppConnected  Xmpp.Session Xmpp.E2EContext [ThreadId]
               | XmppNoConnection

instance Show XmppState where
    show (XmppConnecting tid) = "<Connecting, thread = " ++ show tid ++ " >"
    show XmppConnected{} = "Connected"
    show XmppNoConnection = "Disconnected"

data PSState = PSState { _psDB :: ConnectionPool
                       , _psXmppCon :: TVar XmppState
                       , _psProps :: TMVar PSProperties
                       , _psState :: TVar PontariusState
                       , _psAccountState :: TVar AccountState
                       , _psGpgCreateKeySempahore :: TMVar ThreadId
                       , _psDBusConnection :: TMVar DBusConnection
                       }

newtype PSM m a = PSM {unPSM :: ReaderT PSState m a}
                deriving (Monad, Applicative, Functor, MonadIO, MonadTrans)

deriving instance Monad m => MonadReader PSState (PSM m)


makeLenses ''PSProperties
makeLenses ''PSState

runPSM :: PSState -> PSM m a -> m a
runPSM st (PSM m) = runReaderT m st

getDBusCon :: MonadIO m => PSM m (DBusConnection)
getDBusCon = do
    st <- PSM ask
    liftIO . atomically $ readTMVar $ st ^. psDBusConnection
