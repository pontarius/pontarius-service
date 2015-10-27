{-# LANGUAGE FunctionalDependencies #-}
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

module PontariusService.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           DBus
import           DBus.Signal
import           DBus.Types
import           Data.ByteString (ByteString)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX as Time
import           Data.Typeable
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.Word
import qualified Network.Xmpp as Xmpp


type SSID = ByteString

data BatchLink = BatchLinkIgnore
               | BatchLinkExisting UUID
               | BatchLinkNewContact Text

instance Representable BatchLink where
  type RepType BatchLink = 'TypeStruct '[ 'DBusSimpleType 'TypeByte
                                        , 'DBusSimpleType 'TypeString]
  toRep BatchLinkIgnore = DBVStruct (StructCons
                                    (DBVByte 0) (StructSingleton (DBVString "")))
  toRep (BatchLinkExisting uuid)
    = DBVStruct (StructCons (DBVByte 1) (StructSingleton (toRep uuid)))
  toRep (BatchLinkNewContact name)
    = DBVStruct (StructCons (DBVByte 2) (StructSingleton (toRep name)))
  fromRep
    (DBVStruct (StructCons (DBVByte 0) (StructSingleton _)))
    = Just BatchLinkIgnore
  fromRep (DBVStruct (StructCons (DBVByte 1) (StructSingleton uuid)))
    = BatchLinkExisting <$> (fromRep uuid)
  fromRep (DBVStruct (StructCons (DBVByte 2) (StructSingleton (DBVString name))))
    = Just $ BatchLinkNewContact name


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

data PeerStatus = Unavailable
                | Available
                  deriving (Show, Eq)

instance Representable UTCTime where
    type RepType UTCTime = 'DBusSimpleType 'TypeUInt32
    toRep = DBVUInt32 . round . utcTimeToPOSIXSeconds
    fromRep (DBVUInt32 x) = Just . posixSecondsToUTCTime $ fromIntegral x

instance DBus.Representable Xmpp.Jid where
    type RepType Xmpp.Jid = 'DBusSimpleType 'TypeString
    toRep = DBus.DBVString . Xmpp.jidToText
    fromRep (DBus.DBVString s) = Xmpp.jidFromText s

instance (Ord a, DBus.Representable a) => DBus.Representable (Set a) where
    type RepType (Set a) = RepType [a]
    toRep = toRep . Set.toList
    fromRep = fmap Set.fromList . fromRep

instance Representable (Maybe KeyID) where
    type RepType (Maybe KeyID) = RepType KeyID
    toRep Nothing = toRep Text.empty
    toRep (Just k) = toRep k
    fromRep v = case fromRep v of
        Nothing -> Nothing
        Just v' | Text.null v' -> Just Nothing
                | otherwise -> Just (Just v')

instance Representable (Maybe UTCTime) where
    type RepType (Maybe UTCTime) = RepType UTCTime
    toRep Nothing = toRep (0 :: Word32)
    toRep (Just t) = toRep t
    fromRep v = case fromRep v of
        Nothing -> Nothing
        Just t' | t' == (0 :: Word32) -> Just Nothing
                | otherwise -> Just . Just $ posixSecondsToUTCTime
                               $ fromIntegral t'

type KeyID = Text
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
makeRepresentable ''PeerStatus

instance DBus.Representable UUID where
    type RepType UUID = RepType Text
    toRep = toRep . Text.pack . UUID.toString
    fromRep = UUID.fromString . Text.unpack <=< fromRep

data AddPeerFailed =
    AddPeerFailed { addPeerFailedPeer :: !Xmpp.Jid
                  , addPeerFailedReason :: !Text
                  } deriving (Show)

makeRepresentable ''AddPeerFailed
makeLensesWith camelCaseFields ''AddPeerFailed

data RemovePeerFailed =
    RemovePeerFailed { removePeerFailedPeer :: !Xmpp.Jid
                     , removePeerFailedReason :: !Text
                     } deriving (Show)

makeRepresentable ''RemovePeerFailed
makeLensesWith camelCaseFields ''RemovePeerFailed
