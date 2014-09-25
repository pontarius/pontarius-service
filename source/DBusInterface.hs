{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad
import           DBus as DBus
import           DBus.Types
import           Data.ByteString (ByteString)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Signals
import           Transactions
import           Types
import           Xmpp

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

removeChallengeMethod :: PSState -> DBus.Method
removeChallengeMethod st =
    DBus.Method (DBus.repMethod $ runPSM st . removeChallenge)
    "removeChallenge"
    ("challenge_id" :-> Result )
    ResultDone


initialize st = do
    let stateRef = view psState st
    atomically $ do
        s <- readTVar stateRef
        ps <- getPeersSTM st
        return (s, ps)

initializeMethod :: PSState -> Method
initializeMethod st =
    DBus.Method
    (DBus.repMethod $ initialize st)
    "initialize" Result
    ("state" :> "peers" :> ResultDone)

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


initiateChallengeMethod :: PSState -> Method
initiateChallengeMethod st =
    DBus.Method
    (DBus.repMethod $ \p q s -> runPSM st $ verifyChannel p q s)
    "initiateChallenge" ("peer" :-> "question" :-> "secret" :-> Result)
    ResultDone

respondChallengeMethod :: PSState -> Method
respondChallengeMethod st =
    DBus.Method
    (DBus.repMethod $ (\peer secret -> runPSM st $ respondChallenge peer secret))
    "respondChallenge" ("peer" :-> "secret" :-> Result)
    ResultDone

getTrustStatusMethod :: Method
getTrustStatusMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO Bool))
    "getTrustStatus" ("entity" :-> Result) "is_trusted"

addPeerMethod :: PSState -> Method
addPeerMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . addPeer)
    "addPeer" ("jid" :-> Result)
    ResultDone

removePeerMethod :: PSState -> Method
removePeerMethod st =
    DBus.Method
    (DBus.repMethod $ (runPSM st . removePeer
                       :: Xmpp.Jid -> MethodHandlerT IO ()))
    "removePeer" ("peer" :-> Result)
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
                , initiateChallengeMethod st
                , respondChallengeMethod st
                , removeChallengeMethod st
                , getTrustStatusMethod
                , addPeerMethod st
                , removePeerMethod st
                , registerAccountMethod
                , getIdentitiesMethod
                , setCredentialsMethod st
                , getCredentialsMethod st
                ] []
                [ SSD receivedChallengeSignal
                , SSD challengeResultSignal
                , SSD contactStatusChangedSignal
                , SSD peerTrustStatusChangedSignal
                , SSD subscriptionRequestSignal
                , SSD unlinkedIdentityAvailabilitySignal
                ]
                [ SomeProperty $ identityProp st
                ]


conObject :: PSState -> Object
conObject st = object pontariusInterface (xmppInterface st)

rootObject :: PSState -> Objects
rootObject st = root pontariusObjectPath (conObject st)
