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

module Types where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           DBus
import           DBus.Types
import           DBus.Signal
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX as Time
import           Data.Typeable
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp

data PontariusState = CredentialsUnset
                    | IdentityNotFound
                    | IdentitiesAvailable
                    | CreatingIdentity
                    | Disabled
                    | Authenticating
                    | Authenticated
                    | AuthenticationDenied
                      deriving (Show, Eq)

data AccountState = AccountEnabled
                  | AccountDisabled
                    deriving (Show, Eq)


instance Representable UTCTime where
    type RepType UTCTime = 'DBusSimpleType TypeUInt32
    toRep = DBVUInt32 . round . utcTimeToPOSIXSeconds
    fromRep (DBVUInt32 x) = Just . posixSecondsToUTCTime $ fromIntegral x

instance DBus.Representable Xmpp.Jid where
    type RepType Xmpp.Jid = 'DBusSimpleType TypeString
    toRep = DBus.DBVString . Xmpp.jidToText
    fromRep (DBus.DBVString s) = Xmpp.jidFromText s

data PSProperties = PSProperties{ _pspConnectionStatus :: Property (RepType Bool)}

-- | When requesting a new connection we fork a new thread that creates the
-- connection and sets it's own threadID so it can be aborted when required
data XmppState = XmppConnecting ThreadId
               | XmppConnected  Xmpp.Session
               | XmppNoConnection

instance Show XmppState where
    show (XmppConnecting tid) = "<Connecting, thread = " ++ show tid ++ " >"
    show (XmppConnected _) = "Connected"
    show XmppNoConnection = "Disconnected"

data PSState = PSState { _psDB :: ConnectionPool
                       , _psXmppCon :: TVar XmppState
                       , _psProps :: TMVar PSProperties
                       , _psState :: TVar PontariusState
                       , _psAccountState :: TVar AccountState
                       , _psGpgCreateKeySempahore :: TMVar ThreadId
                       }

newtype PSM m a = PSM {unPSM :: ReaderT PSState m a}
                deriving (Monad, Applicative, Functor, MonadIO, MonadTrans)

deriving instance Monad m => MonadReader PSState (PSM m)

runPSM :: PSState -> PSM m a -> m a
runPSM st (PSM m) = runReaderT m st


type KeyID = ByteString
type SessionID = ByteString

data ConnectionStatus = Connected
                      | Disconnected
                      deriving (Show, Eq, Ord)

data InitResponse = KeyOK
                  | SelectKey

data Ent = Ent { entityJid :: Xmpp.Jid
               , entityDisplayName :: Text
               , entityDescription :: Text
               } deriving (Show, Typeable)

data AkeEvent = AkeEvent { akeEventStart :: UTCTime
                         , akeEventSuccessfull :: Bool
                         , akeEventPeerJid :: Xmpp.Jid
                         , akeEventOurJid :: Xmpp.Jid
                         , akeEventPeerkeyID :: KeyID
                         , akeEventOurkeyID :: KeyID
                         } deriving (Show, Eq)

data ChallengeEvent =
    ChallengeEvent { challengeEventChallengeOutgoing :: Bool
                   , challengeEventStarted :: UTCTime
                   , challengeEventCompleted :: UTCTime
                   , challengeEventQuestion :: Text
                   , challengeEventResult :: Text
                   } deriving (Show, Eq)

data RevocationEvent =
    RevocationEvent { revocationEventKeyID :: KeyID
                    , revocationEventTime :: UTCTime
                    }

data RevocationSignalEvent =
    RevocationlEvent { revocationSignalEventKeyID :: KeyID
                          , revocationSignalEventTime :: UTCTime
                          }

makeLenses ''PSProperties
makeLenses ''PSState

makePrisms ''PontariusState
makeRepresentable ''PontariusState

makePrisms ''AccountState
makeRepresentable ''AccountState

makeRepresentable ''RevocationSignalEvent
makeRepresentable ''ConnectionStatus
makeRepresentable ''InitResponse
makeRepresentable ''Ent
makeRepresentable ''AkeEvent
makeRepresentable ''ChallengeEvent
makeRepresentable ''RevocationEvent

class Signaling m where
    signals :: Signal -> m ()

instance Signaling (MethodHandlerT IO) where
    signals = signal

instance (MonadTrans t, MonadReader DBusConnection (t IO)) => Signaling (t IO) where
    signals s = do
        dbc <- ask
        lift $ emitSignal s dbc
