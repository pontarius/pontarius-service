{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module DBusInterface
   (rootObject) where

import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Lens
import           Control.Monad.Trans
import           DBus as DBus
import           DBus.Types
import           Data.ByteString (ByteString)
import           Data.Proxy
import           Data.Singletons
import           Data.String
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Transactions
import           Types

data Stub = Stub deriving (Show, Typeable)

instance Ex.Exception Stub

class IsStub a where
    stub :: a

instance IsStub (IO a) where
    stub = Ex.throwIO Stub

instance IsStub (MethodHandlerT IO a) where
    stub = methodError $ MsgError "org.pontarius.Error.Stub" Nothing []

instance IsStub b => IsStub (a -> b) where
    stub = \_ -> stub

pontariusProperty :: SingI t => Text -> Property t
pontariusProperty name =
    Property { propertyName = name
             , propertyPath = pontariusObjectPath
             , propertyInterface = pontariusInterface
             , propertyGet = Just stub
             , propertySet = Nothing
             , propertyEmitsChangedSignal = PECSTrue
             }
----------------------------------------------------
-- Methods
----------------------------------------------------

instance IsString (ResultDescription ('Arg 'Null)) where
    fromString t = (fromString t :> ResultDone)

setCredentialsMethod :: PSState -> Method
setCredentialsMethod st =
    Method (DBus.repMethod $ \u p -> setCredentialsM st u p)
           "setCredentials"
           ("username" :-> "password" :-> Result)
           ResultDone

getCredentialsMethod :: PSState -> Method
getCredentialsMethod st =
    Method (DBus.repMethod $ getCredentialsM st)
           "getCredentials"
           Result
           ("username" :> ResultDone)

securityHistoryByJidMethod :: Method
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


securityHistoryByKeyIdMethod :: Method
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

initialize :: PSState -> IO PontariusState
initialize st = runPSM st $ liftIO . atomically . readTVar =<< view psState

initializeMethod :: PSState -> Method
initializeMethod st =
    DBus.Method
    (DBus.repMethod $ initialize st)
    "initialize" Result
    ("result" :> ResultDone)

importKeyMethod :: Method
importKeyMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO KeyID ))
    "importKey" ("location" :-> Result) "key_id"

markKeyVerifiedMethod :: Method
markKeyVerifiedMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: KeyID -> IO () ))
    "markKeyVerified" ("key-id" :-> Result) ResultDone

revokeIdentityMethod :: Method
revokeIdentityMethod =
    DBus.Method
    (DBus.repMethod $ (revokeIdentity ::  KeyID -> MethodHandlerT IO ()))
    "revokeIdentity" ("key_id" :-> Result) ResultDone

createIdentityMethod :: PSState -> Method
createIdentityMethod st =
    DBus.Method
    (DBus.repMethod $ (runPSM st synchronousCreateGpgKey
                           :: MethodHandlerT IO ByteString))
    "createIdentity"
    Result
    ("key_id" -- key_id of the newly created key
     :> ResultDone)


initiateChallengeMethod :: Method
initiateChallengeMethod =
    DBus.Method
    (DBus.repMethod $ (stub ::  Xmpp.Jid -> Text -> Text -> IO Text))
    "initiateChallenge" ("peer" :-> "question" :-> "secret" :-> Result)
    "challenge_id"

respondChallengeMethod :: Method
respondChallengeMethod =
    DBus.Method
    (DBus.repMethod $ (stub ::  Text -> Text -> IO ()))
    "respondChallenge" ("challenge_id" :-> "secret" :-> Result)
    ResultDone

getTrustStatusMethod :: Method
getTrustStatusMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO Bool))
    "getTrustStatus" ("entity" :-> Result) "is_trusted"

getEntityPubkeyMethod :: Method
getEntityPubkeyMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO Text))
    "getEntityPubkey" ("entity" :-> Result)
    "key_id"

addPeerMethod :: Method
addPeerMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> Text -> IO ()))
    "addPeer" ("jid" :-> "name" :-> Result)
    ResultDone

removePeerMethod :: Method
removePeerMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ()))
    "removePeer" ("entity" :-> Result)
    ResultDone

registerAccountMethod :: Method
registerAccountMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> Text -> Text -> IO ()))
    "registerAccount" ("server" :-> "username" :-> "password"
                        :-> Result)
    ResultDone


setIdentityMethod :: PSState -> Method
setIdentityMethod st =
    DBus.Method (DBus.repMethod $ setSigningGpgKeyM st)
                "setIdentity"
                ("keyID" :-> Result)
                ResultDone

sArgument :: SingI t => Text -> Proxy (t :: DBusType) -> SignalArgument
sArgument name (Proxy :: Proxy (t :: DBusType)) =
    SignalArgument { signalArgumentName = name
                   , signalArgumentType = fromSing (sing :: Sing t)
                   }

receivedChallengeSignal :: SignalInterface
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

challengeResultSignal :: SignalInterface
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

challengeTimeoutSignal :: SignalInterface
challengeTimeoutSignal = SignalI { signalName = "challengeTimeout"
                                , signalArguments =
                                    [ sArgument "peer"
                                      (Proxy :: Proxy (RepType Xmpp.Jid))
                                    , sArgument "challenge_id"
                                      (Proxy :: Proxy (RepType Text))
                                    ]
                                , signalAnnotations = []
                                }

peerStatusChangeSignal :: SignalInterface
peerStatusChangeSignal = SignalI { signalName = "peerStatusChanged"
                                 , signalArguments =
                                    [ sArgument "peer"
                                      (Proxy :: Proxy (RepType Xmpp.Jid))
                                    ,  sArgument "status"
                                      (Proxy :: Proxy (RepType Text))
                                    ]
                                , signalAnnotations = []
                                }

peerTrustStatusChangeSignal :: SignalInterface
peerTrustStatusChangeSignal = SignalI { signalName = "peerTrustStatusChanged"
                                      , signalArguments =
                                          [ sArgument "peer"
                                            (Proxy :: Proxy (RepType Xmpp.Jid))
                                          ,  sArgument "trust_status"
                                             (Proxy :: Proxy (RepType Text))
                                          ]
                                      , signalAnnotations = []
                                      }

subscriptionRequestSignal :: SignalInterface
subscriptionRequestSignal = SignalI { signalName = "subscriptionRequest"
                                    , signalArguments =
                                        [ sArgument "peer"
                                          (Proxy :: Proxy (RepType Xmpp.Jid))
                                        ]
                                    , signalAnnotations = []
                                    }

availableEntitiesProperty :: Property (RepType [Ent])
availableEntitiesProperty = pontariusProperty "AvailableEntities"

unavailanbleEntitiesProperty :: Property (RepType [Ent])
unavailanbleEntitiesProperty = pontariusProperty "UnvailableEntities"



----------------------------------------------------
-- Objects
----------------------------------------------------

xmppInterface :: PSState -> Interface
xmppInterface st = Interface
                [ importKeyMethod
                , createIdentityMethod st
                , initializeMethod st
                , markKeyVerifiedMethod
                , securityHistoryByJidMethod
                , securityHistoryByKeyIdMethod
                , setIdentityMethod st
                , revokeIdentityMethod
                , initiateChallengeMethod
                , respondChallengeMethod
                , getTrustStatusMethod
                , getEntityPubkeyMethod
                , addPeerMethod
                , removePeerMethod
                , registerAccountMethod
                , getIdentitiesMethod
                , setCredentialsMethod st
                , getCredentialsMethod st
                ] []
                [ receivedChallengeSignal
                , challengeResultSignal
                , challengeTimeoutSignal
                , peerStatusChangeSignal
                , peerTrustStatusChangeSignal
                , subscriptionRequestSignal
                ]
                [ SomeProperty availableEntitiesProperty
                , SomeProperty unavailanbleEntitiesProperty
                , SomeProperty $ identityProp st
                ]


conObject :: PSState -> Object
conObject st = object pontariusInterface (xmppInterface st)

rootObject :: PSState -> Objects
rootObject st = root pontariusObjectPath (conObject st)
