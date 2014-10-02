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
import           DBus as DBus
import           DBus.Types
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.UUID (UUID)
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Persist.Schema
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

instance IsString (ArgumentDescription ('Arg 'Null)) where
    fromString t = (fromString t :> Done)

setCredentialsMethod :: PSState -> Method
setCredentialsMethod st =
    Method (DBus.repMethod $ \u p -> setCredentialsM st u p)
           "setCredentials"
           ("username" :> "password" :> Done)
           Done

getCredentialsMethod :: PSState -> Method
getCredentialsMethod st =
    Method (DBus.repMethod $ getCredentialsM st)
           "getCredentials"
           Done
           ("username" :> Done)

securityHistoryByJidMethod :: Method
securityHistoryByJidMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
                                              , [ChallengeEvent]
                                              , [RevocationEvent]
                                              , [RevocationSignalEvent]
                                              )))
    "securityHistoryByJID" ("peer" :> Done )
    ( "ake_events"
      :> "challenge_events"
      :> "revocation_events"
      :> "revocation_signal_events"
      :> Done
    )


securityHistoryByKeyIdMethod :: Method
securityHistoryByKeyIdMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
                                         , [ChallengeEvent]
                                         , [RevocationEvent]
                                         , [RevocationSignalEvent]
                                         )))
    "securityHistoryByKeyID" ("key_id" :> Done)
    ("ake_events"
     :> "challenge_events"
     :> "revocation_events"
     :> "revocation_signal_events"
     :> Done
    )

removeChallengeMethod :: PSState -> DBus.Method
removeChallengeMethod st =
    DBus.Method (DBus.repMethod $ runPSM st . removeChallenge)
    "removeChallenge"
    ("challenge_id" :> Done )
    Done


initialize :: PSState
           -> IO ( PontariusState
                 , Map UUID (Text, Set Xmpp.Jid), [Xmpp.Jid])
initialize st = do
    let stateRef = view psState st
    (cs, us) <- availableContacts st
    s <- readTVarIO stateRef
    return (s, contactsMap cs, us)
  where
    contactsMap = Map.fromList
                  . map (\(c, js) -> (contactUniqueID c, (contactName c, js)))
                  . Map.toList

initializeMethod :: PSState -> Method
initializeMethod st =
    DBus.Method
    (DBus.repMethod $ initialize st)
    "initialize" Done
    ( "state"
     :> "available_contacts"
     :> "unlinked-peers"
     :> Done)

markKeyVerifiedMethod :: PSState -> Method
markKeyVerifiedMethod st =
    DBus.Method
    (DBus.repMethod $ (\kid isV -> runPSM st $ setKeyVerifiedM kid isV ))
    "identityVerified" ("key_id" :> "is_verified" :> Done) Done

revokeIdentityMethod :: Method
revokeIdentityMethod =
    DBus.Method
    (DBus.repMethod $ (revokeIdentity ::  KeyID -> MethodHandlerT IO ()))
    "revokeIdentity" ("key_id" :> Done) Done

createIdentityMethod :: PSState -> Method
createIdentityMethod st =
    DBus.Method
    (DBus.repMethod $ (runPSM st synchronousCreateGpgKey
                           :: MethodHandlerT IO ByteString))
    "createIdentity"
    Done
    ("key_id" -- key_id of the newly created key
     :> Done)

initiateChallengeMethod :: PSState -> Method
initiateChallengeMethod st =
    DBus.Method
    (DBus.repMethod $ \p q s -> runPSM st $ verifyChannel p q s)
    "initiateChallenge" ("peer" :> "question" :> "secret" :> Done)
    Done

respondChallengeMethod :: PSState -> Method
respondChallengeMethod st =
    DBus.Method
    (DBus.repMethod $ (\peer secret -> runPSM st $ respondChallenge peer secret))
    "respondChallenge" ("peer" :> "secret" :> Done)
    Done

getTrustStatusMethod :: Method
getTrustStatusMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> IO Bool))
    "getTrustStatus" ("entity" :> Done) "is_trusted"

addPeerMethod :: PSState -> Method
addPeerMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . addPeer)
    "addPeer" ("jid" :> Done)
    Done

removePeerMethod :: PSState -> Method
removePeerMethod st =
    DBus.Method
    (DBus.repMethod $ (runPSM st . removePeer
                       :: Xmpp.Jid -> MethodHandlerT IO ()))
    "removePeer" ("peer" :> Done)
    Done

registerAccountMethod :: Method
registerAccountMethod =
    DBus.Method
    (DBus.repMethod $ (stub :: Text -> Text -> Text -> IO ()))
    "registerAccount" ("server" :> "username" :> "password"
                        :> Done)
    Done

setIdentityMethod :: PSState -> Method
setIdentityMethod st =
    DBus.Method (DBus.repMethod $ setSigningGpgKeyM st)
                "setIdentity"
                ("keyID" :> Done)
                Done

newContactMethod :: PSState -> Method
newContactMethod st =
    DBus.Method (DBus.repMethod $ newContactM st)
                "newContact"
                ("name" :> Done)
                "contact_id"

linkIdentityMethod :: PSState -> Method
linkIdentityMethod st =
    DBus.Method (DBus.repMethod $ \kid c -> moveIdentity st kid (Just c) )
                "linkIdentity"
                ("identity" :> "contact" :> Done)
                Done

unlinkIdentityMethod :: PSState -> Method
unlinkIdentityMethod st =
    DBus.Method (DBus.repMethod $ \kid -> moveIdentity st kid Nothing )
                "unlinkIdentity"
                ("identity" :> Done)
                Done

----------------------------------------------------
-- Objects
----------------------------------------------------

xmppInterface :: PSState -> Interface
xmppInterface st = Interface
                [ createIdentityMethod st
                , initializeMethod st
                , markKeyVerifiedMethod st
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
                , newContactMethod st
                , linkIdentityMethod st
                , unlinkIdentityMethod st
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
