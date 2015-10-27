{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Signals where

import           DBus
import           DBus.Types
import           Data.Singletons
import           Data.UUID (UUID)
import qualified Network.Xmpp as Xmpp
import           Data.Text (Text)

import           Basic
import           Types

type SigD a = SignalDescription (FlattenRepType (RepType a))

pontariusSignal :: SingI a =>
                   Text
                -> ArgumentDescription (ArgParity a)
                -> SignalDescription a
pontariusSignal name args =
        SignalDescription { signalDMember = name
                          , signalDInterface = pontariusInterface
                          , signalDPath = pontariusObjectPath
                          , signalDArguments = args
                          }

receivedChallengeSignal :: SigD (Text, Text)
receivedChallengeSignal = pontariusSignal "receivedChallenge"
                          ("peer" :> "question" :> Done)


challengeResultSignal :: SigD (Text, Text, Text, Bool)
challengeResultSignal = pontariusSignal "challengeResult"
                        ( "peer"
                        :> "challenge_id"
                        :> "initiator"
                        :> "result"
                        :> Done
                        )

contactStatusChangedSignal :: SigD (UUID, Text, PeerStatus)
contactStatusChangedSignal = pontariusSignal "contactStatusChanged"
                             ( "contact"
                             :> "contact_name"
                             :> "status"
                             :> Done
                             )

peerTrustStatusChangedSignal :: SigD (Text, Bool)
peerTrustStatusChangedSignal = pontariusSignal "peerTrustStatusChanged"
                              ( "peer"
                              :> "trust_status"
                              :> Done
                              )
subscriptionRequestSignal :: SigD Text
subscriptionRequestSignal = pontariusSignal "subscriptionRequest"
                                            ("peer" :> Done)


unlinkedIdentityAvailabilitySignal :: SigD (Text, Xmpp.Jid , PeerStatus)
unlinkedIdentityAvailabilitySignal =
    pontariusSignal "unlinkedIdentityStatusChanged"
                    ("identity" :> "peer" :> "status" :> Done)

identityAvailabilitySignal :: SigD (Text, Xmpp.Jid, UUID, PeerStatus)
identityAvailabilitySignal =
    pontariusSignal "identityStatusChanged"
                    ("identity" :> "peer" :> "contact" :> "status" :> Done)

identityContactMovedSignal :: SigD (Text, UUID)
identityContactMovedSignal =
    pontariusSignal "identityContactMovedSignal"
                    ("identity" :> "contact" :> Done)

identityUnlinkedSignal :: SigD (Text)
identityUnlinkedSignal =
    pontariusSignal "identityUnlinkedSignal"
                    ("identity"  :> Done)

contactRemovedSignal :: SigD UUID
contactRemovedSignal = pontariusSignal "contactRemoved"
                       ("contact" :> Done)


contactRenamedSignal :: SigD (UUID, Text)
contactRenamedSignal = pontariusSignal "contactRenamed"
                       ("contact" :> "name" :> Done)

addPeersFailedSignal :: SigD [AddPeerFailed] -- Jid and Reason
addPeersFailedSignal = pontariusSignal "addPeersFailed"
                      ("peers and reason" :> Done)

removePeersFailedSignal :: SigD [RemovePeerFailed] -- Jid and Reason
removePeersFailedSignal = pontariusSignal "removePeersFailed"
                          ("peers and reasons" :> Done)
