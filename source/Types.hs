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
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           DBus
import           DBus.Signal
import           DBus.Types
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
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

class MonadMethodError m where
    throwMethodError :: MsgError -> m a

instance Monad m => MonadMethodError (MethodHandlerT m) where
    throwMethodError = methodError

instance MonadMethodError IO where
    throwMethodError = Ex.throwIO

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

data PSCallbacks = PSCallbacks { _onStateChange :: !(XmppState -> PSM IO ())
                               }

data PSState = PSState { _psDB                    :: !ConnectionPool
                       , _psXmppCon               :: !(TVar XmppState)
                       , _psProps                 :: !(TMVar PSProperties)
                       , _psState                 :: !(TVar PontariusState)
                       , _psAccountState          :: !(TVar AccountState)
                       , _psGpgCreateKeySempahore :: !(TMVar ThreadId)
                       , _psDBusConnection        :: !(TMVar DBusConnection)
                       , _psSubscriptionRequests  :: !(TVar (Set Xmpp.Jid))
                       , _psCallbacks             :: !PSCallbacks
                       }

newtype PSM m a = PSM {unPSM :: ReaderT PSState m a}
                deriving (Monad, Applicative, Functor, MonadIO, MonadTrans
                         , MonadThrow, MonadCatch
                         )

io :: IO a -> IO a
io = id

methodHandler :: MethodHandlerT IO a -> MethodHandlerT IO a
methodHandler = id

deriving instance Monad m => MonadReader PSState (PSM m)


makeLenses ''PSProperties
makeLenses ''PSCallbacks
makeLenses ''PSState

addSubscriptionRequest :: MonadIO m => Xmpp.Jid -> PSM m ()
addSubscriptionRequest fr = do
    srs <- PSM $ view psSubscriptionRequests
    liftIO . atomically $ modifyTVar srs $ Set.insert fr

removeSubscriptionRequest :: MonadIO m => Xmpp.Jid -> PSM m ()
removeSubscriptionRequest fr = do
    srs <- PSM $ view psSubscriptionRequests
    liftIO . atomically $ modifyTVar srs $ Set.delete fr

getSubscriptionRequests :: MonadIO m => PSM m (Set Xmpp.Jid)
getSubscriptionRequests = do
    srs <- PSM $ view psSubscriptionRequests
    liftIO . atomically $ readTVar srs

runPSM :: PSState -> PSM m a -> m a
runPSM st (PSM m) = runReaderT m st

getDBusCon :: MonadIO m => PSM m (DBusConnection)
getDBusCon = do
    st <- PSM ask
    liftIO . atomically $ readTMVar $ st ^. psDBusConnection

instance Representable (Maybe UUID) where
  type RepType (Maybe UUID) = RepType Text
  toRep Nothing = toRep ("" :: Text)
  toRep (Just x) = toRep x
  fromRep v = case fromRep v of
               Nothing -> Nothing
               Just "" -> Just Nothing
               Just txt -> Just <$> UUID.fromText txt
