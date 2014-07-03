{-# OPTIONS_GHC  -fno-warn-incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           DBus
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX as Time
import           Data.Typeable
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp


instance Representable UTCTime where
    type RepType UTCTime = 'DBusSimpleType TypeUInt32
    toRep = DBVUInt32 . round . utcTimeToPOSIXSeconds
    fromRep (DBVUInt32 x) = Just . posixSecondsToUTCTime $ fromIntegral x

instance DBus.Representable Xmpp.Jid where
    type RepType Xmpp.Jid = 'DBusSimpleType TypeString
    toRep = DBus.DBVString . Xmpp.jidToText
    fromRep (DBus.DBVString s) = Xmpp.jidFromText s

data PSProperties = PSProperties{ _pspConnectionStatus :: Property (RepType Bool)}

data PSState = PSState { _psDB :: ConnectionPool
                       , _psXmppCon :: TMVar Xmpp.Session
                       , _psProps :: TMVar PSProperties
                       }

newtype PSM m a = PSM {unPSM :: ReaderT PSState m a}
                deriving (Monad, Applicative, Functor, MonadIO, MonadTrans)

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
    RevocationSignalEvent { revocationSignalEventKeyID :: KeyID
                          , revocationSignalEventTime :: UTCTime
                          }


makeLenses ''PSProperties
makeLenses ''PSState

makeRepresentable ''RevocationSignalEvent
makeRepresentable ''ConnectionStatus
makeRepresentable ''InitResponse
makeRepresentable ''Ent
makeRepresentable ''AkeEvent
makeRepresentable ''ChallengeEvent
makeRepresentable ''RevocationEvent
