{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module DBusInterface where

import qualified Control.Exception as Ex
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           DBus as DBus
import           DBus.Introspect as DBus
import           DBus.Types
import           Data.Proxy
import           Data.Singletons
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Typeable (Typeable)
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Persist
import           Types

data Stub = Stub deriving (Show, Typeable)

instance Ex.Exception Stub

class IsStub a where
    stub :: a

instance IsStub (IO a) where
    stub = Ex.throwIO Stub

instance IsStub (MethodHandlerT IO a) where
    stub = lift $ Ex.throwIO Stub

instance IsStub b => IsStub (a -> b) where
    stub = \_ -> stub

type PropSetterStub a = DBusValue (RepType a) -> MethodHandlerT IO Bool
type PropGetterStub a = MethodHandlerT IO (DBusValue (RepType a))

pontariusProperty name =
    Property { propertyName = name
             , propertyPath = pontariusObjectPath
             , propertyInterface = pontariusInterface
             , propertyGet = Just stub
             , propertySet = Just stub
             , propertyEmitsChangedSignal = PECSTrue
             }

----------------------------------------------------
-- Methods
----------------------------------------------------

instance IsString (ResultDescription ('Arg 'Null)) where
    fromString t = (fromString t :> ResultDone)

securityHistoryByJidMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
                                              , [ChallengeEvent]
                                              , [RevocationEvent]
                                              , [RevocationSignalEvent]
                                              )))
    "securityHistoryByJID" ("peer" :-> Result )
    ( "ake_events"
      :> "challenge_events"
      :> "revocation_events"
      :> "revocation_signal_events"
      :> ResultDone
    )


securityHistoryByKeyIdMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
                                         , [ChallengeEvent]
                                         , [RevocationEvent]
                                         , [RevocationSignalEvent]
                                         )))
    "securityHistoryByKeyID" ("key_id" :-> Result)
    ("ake_events"
     :> "challenge_events"
     :> "revocation_events"
     :> "revocation_signal_events"
     :> ResultDone
    )

initializeMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: IO InitResponse ))
    "initialize" Result
    ("result" :> ResultDone)

importKeyMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO KeyID ))
    "importKey" ("location" :-> Result) "key_id"

markKeyVerifiedMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: KeyID -> IO () ))
    "markKeyVerified" ("key-id" :-> Result) ResultDone

revokeKeymethod =
    DBus.Method
    (DBus.repMethod $ (stub ::  KeyID -> Text -> IO ()))
    "revokeKey" ("key_id" :-> "reason" :-> Result) ResultDone

initiateChallengeMethod =
    DBus.Method
    (DBus.repMethod $ (stub ::  Xmpp.Jid -> Text -> Text -> IO Text))
    "initiateChallenge" ("peer" :-> "question" :-> "secret" :-> Result)
    "challenge_id"

respondChallengeMethod =
    DBus.Method
    (DBus.repMethod $ (stub ::  Text -> Text -> IO ()))
    "respondChallenge" ("challenge_id" :-> "secret" :-> Result)
    ResultDone

getTrustStatusMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO Bool))
    "getTrustStatus" ("entity" :-> Result) "is_trusted"

getEntityPubkeyMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO Text))
    "getEntityPubkey" ("entity" :-> Result)
    "key_id"

addPeerMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> Text -> IO ()))
    "addPeer" ("jid" :-> "name" :-> Result)
    ResultDone

removePeerMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ()))
    "removePeer" ("entity" :-> Result)
    ResultDone

registerAccountMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> Text -> Text -> IO ()))
    "registerAccount" ("server" :-> "username" :-> "password"
                        :-> Result)
    ResultDone

sArgument name (Proxy :: Proxy (t :: DBusType)) =
    SignalArgument { signalArgumentName = name
                   , signalArgumentType = fromSing (sing :: Sing t)
                   }

receivedChallengeSignal = SignalI { signalName = "receivedChallenge"
                                  , signalArguments =
                                      [ sArgument "peer"
                                          (Proxy :: Proxy (RepType Xmpp.Jid))
                                      , sArgument "challenge_id"
                                          (Proxy :: Proxy (RepType Text))
                                      , sArgument "question"
                                          (Proxy :: Proxy (RepType Text))
                                      ]

                                  , signalAnnotations = []
                                  }

challengeResultSignal = SignalI { signalName = "challengeResult"
                                , signalArguments =
                                    [ sArgument "peer"
                                      (Proxy :: Proxy (RepType Xmpp.Jid))
                                    , sArgument "challenge_id"
                                      (Proxy :: Proxy (RepType Text))
                                    , sArgument "initiator"
                                      (Proxy :: Proxy (RepType Text))
                                    , sArgument "result"
                                      (Proxy :: Proxy (RepType Bool))
                                    ]
                                , signalAnnotations = []
                                }

challengeTimeoutSignal = SignalI { signalName = "challengeTimeout"
                                , signalArguments =
                                    [ sArgument "peer"
                                      (Proxy :: Proxy (RepType Xmpp.Jid))
                                    , sArgument "challenge_id"
                                      (Proxy :: Proxy (RepType Text))
                                    ]
                                , signalAnnotations = []
                                }

peerStatusChangeSignal = SignalI { signalName = "peerStatusChanged"
                                 , signalArguments =
                                    [ sArgument "peer"
                                      (Proxy :: Proxy (RepType Xmpp.Jid))
                                    ,  sArgument "status"
                                      (Proxy :: Proxy (RepType Text))
                                    ]
                                , signalAnnotations = []
                                }

peerTrustStatusChangeSignal = SignalI { signalName = "peerTrustStatusChanged"
                                      , signalArguments =
                                          [ sArgument "peer"
                                            (Proxy :: Proxy (RepType Xmpp.Jid))
                                          ,  sArgument "trust_status"
                                             (Proxy :: Proxy (RepType Text))
                                          ]
                                      , signalAnnotations = []
                                      }


connectionStatusProperty :: Property (RepType Text)
connectionStatusProperty = pontariusProperty "ConnectionStatus"

hostProperty :: Property (RepType Text)
hostProperty = pontariusProperty "Host"

usernameProperty :: Property (RepType Text)
usernameProperty = pontariusProperty "Username"

passwordProperty :: Property (RepType Text)
passwordProperty = pontariusProperty "Password"

availableEntitiesProperty :: Property (RepType [Ent])
availableEntitiesProperty = pontariusProperty "AvailableEntities"

unavailanbleEntitiesProperty :: Property (RepType [Ent])
unavailanbleEntitiesProperty = pontariusProperty "UnvailableEntities"

keyProperty :: Property (RepType KeyID)
keyProperty = pontariusProperty "SigningKey"



----------------------------------------------------
-- Objects
----------------------------------------------------

xmppInterface st = Interface
                [ importKeyMethod
                , createKeyMethod st
                , initializeMethod
                , markKeyVerifiedMethod
                , securityHistoryByJidMethod
                , securityHistoryByKeyIdMethod
                , revokeKeymethod
                , initiateChallengeMethod
                , respondChallengeMethod
                , getTrustStatusMethod
                , getEntityPubkeyMethod
                , addPeerMethod
                , removePeerMethod
                , registerAccountMethod
                ] []
                [ receivedChallengeSignal
                , challengeResultSignal
                , challengeTimeoutSignal
                , peerStatusChangeSignal
                , peerTrustStatusChangeSignal
                ]
                [ SomeProperty connectionStatusProperty
                , SomeProperty hostProperty
                , SomeProperty usernameProperty
                , SomeProperty passwordProperty
                , SomeProperty availableEntitiesProperty
                , SomeProperty unavailanbleEntitiesProperty
                , SomeProperty keyProperty
                ]


conObject st = object pontariusInterface (xmppInterface st)

rootObject st = root pontariusObjectPath (conObject st)
