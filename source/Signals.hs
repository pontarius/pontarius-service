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

contactStatusChangedSignal :: SigD (UUID, PeerStatus)
contactStatusChangedSignal = pontariusSignal "contactStatusChanged"
                             ( "contact"
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

identityContactMovedSignal :: SigD (Text, UUID)
identityContactMovedSignal =
    pontariusSignal "identityContactMovedSignal"
                    ("identity" :> "contact" :> Done)

identityUnlinkedSignal :: SigD (Text)
identityUnlinkedSignal =
    pontariusSignal "identityUnlinkedSignal"
                    ("identity"  :> Done)
