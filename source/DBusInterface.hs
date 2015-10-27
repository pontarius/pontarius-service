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

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens hiding ((:>))
import           DBus as DBus
import           DBus.Types
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.String
import qualified Network.Xmpp as Xmpp

import           Basic
import           Gpg
import           Persist
import           Signals
import           Transactions
import           Types
import           Xmpp

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

-- securityHistoryByJidMethod :: Method
-- securityHistoryByJidMethod =
--     DBus.Method
--     (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
--                                               , [ChallengeEvent]
--                                               , [RevocationEvent]
--                                               , [RevocationSignalEvent]
--                                               )))
--     "securityHistoryByJID" ("peer" :> Done )
--     ( "ake_events"
--       :> "challenge_events"
--       :> "revocation_events"
--       :> "revocation_signal_events"
--       :> Done
--     )


-- securityHistoryByKeyIdMethod :: Method
-- securityHistoryByKeyIdMethod =
--     DBus.Method
--     (DBus.repMethod $ (stub :: Xmpp.Jid -> IO ( [AkeEvent]
--                                          , [ChallengeEvent]
--                                          , [RevocationEvent]
--                                          , [RevocationSignalEvent]
--                                          )))
--     "securityHistoryByKeyID" ("key_id" :> Done)
--     ("ake_events"
--      :> "challenge_events"
--      :> "revocation_events"
--      :> "revocation_signal_events"
--      :> Done
--     )

removeChallengeMethod :: PSState -> DBus.Method
removeChallengeMethod st =
    DBus.Method (DBus.repMethod $ runPSM st . removeChallenge)
    "removeChallenge"
    ("challenge_id" :> Done )
    Done

initialize :: PSState -> IO PontariusState
initialize st = do
    let stateRef = view psState st
    readTVarIO stateRef

initializeMethod :: PSState -> Method
initializeMethod st =
    DBus.Method
    (DBus.repMethod $ initialize st)
    "initialize" Done
    ( "state"
     :> Done)

getUnlinkedPeers :: PSState -> IO (Map Xmpp.Jid KeyID)
getUnlinkedPeers st = snd <$> availableContacts st

getUnlinkedPeersMethod :: PSState -> Method
getUnlinkedPeersMethod st =
    DBus.Method
    (DBus.repMethod $ getUnlinkedPeers st)
    "getUnlinkedPeers" Done
    ( "unlinkedPeers"
     :> Done)

markKeyVerifiedMethod :: PSState -> Method
markKeyVerifiedMethod st =
    DBus.Method
    (DBus.repMethod $ (\kid isV -> runPSM st $ setKeyVerifiedM kid isV ))
    "identityVerified" ("key_id" :> "is_verified" :> Done) Done

revokeIdentityMethod :: PSState -> Method
revokeIdentityMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . revokeIdentity)
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

getIdentityChallengesMethod :: PSState -> Method
getIdentityChallengesMethod st =
    DBus.Method
    (DBus.repMethod $ getIdentityChallengesM st)
    "getIdentityChallenges"
    ("key_id" :> Done) ("challenges" :> Done)

getTrustStatusMethod :: PSState -> Method
getTrustStatusMethod st =
    DBus.Method
    (DBus.repMethod $ isKeyVerifiedM st)
    "keyTrustStatus" ("key_id" :> Done) "is_trusted"

addPeerMethod :: PSState -> Method
addPeerMethod st =
    DBus.Method
    (DBus.repMethod $ methodHandler . runPSM st . addPeerM)
    "addPeer" ("jid" :> Done)
    Done

removePeerMethod :: PSState -> Method
removePeerMethod st =
    DBus.Method
    (DBus.repMethod $ (runPSM st . removePeer
                       :: Xmpp.Jid -> MethodHandlerT IO ()))
    "removePeer" ("peer" :> Done)
    Done

-- registerAccountMethod :: Method
-- registerAccountMethod =
--     DBus.Method
--     (DBus.repMethod $ (stub :: Text -> Text -> Text -> IO ()))
--     "registerAccount" ("server" :> "username" :> "password"
--                         :> Done)
--     Done

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

getContactsMethod :: PSState -> Method
getContactsMethod st =
        DBus.Method (DBus.repMethod $ runPSM st getContactsM)
        "getContacts"
        Done
        ("contacts" :> Done)


removeContactMethod :: PSState -> Method
removeContactMethod st =
        DBus.Method (DBus.repMethod $ runPSM st . removeContactM)
        "removeContacts"
        ("contact" :> Done)
        Done

renameContactMethod :: PSState -> Method
renameContactMethod st =
        DBus.Method (DBus.repMethod $ \c name -> runPSM st
                                                  $ renameContactM c name)
        "renameContact"
        ("contact" :> "name" :> Done)
        Done

getContactPeersMethod :: PSState -> Method
getContactPeersMethod st =
        DBus.Method (DBus.repMethod $ runPSM st . Xmpp.getContactPeers)
        "getContactPeers"
        ("contact" :> Done)
        ("peers" :> Done)

getContactIdentitiesMethod :: PSState -> Method
getContactIdentitiesMethod st =
        DBus.Method (DBus.repMethod $ runPSM st . getContactIdentities)
        "getContactIdentities"
        ("contact" :> Done)
        ("identities" :> Done)

getSessionByJIDMethod :: PSState -> Method
getSessionByJIDMethod st =
    DBus.Method
    (DBus.repMethod $ io . runPSM st . getJidSessions)
    "getSessionsByJID"
    ("jid" :> Done)
    ("sessions" :> Done)

getSessionByIdentityMethod :: PSState -> Method
getSessionByIdentityMethod st =
    DBus.Method
    (DBus.repMethod $ io . runPSM st . getIdentitySessions)
    "getSessionsByIdentity"
    ("identity" :> Done)
    ("sessions" :> Done)

ignorePeerMethod :: PSState -> Method
ignorePeerMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . ignorePeer)
    "ignorePeer"
    ("jid" :> Done)
    Done

unignorePeerMethod :: PSState -> Method
unignorePeerMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . unignorePeer)
    "unignorePeer"
    ("jid" :> Done)
    Done

linkPeersContactsMethod :: PSState -> Method
linkPeersContactsMethod st =
    DBus.Method
    (DBus.repMethod $ runPSM st . batchLink)
    "linkPeers"
    ("jids and " :> Done)
    Done

----------------------------------------------------
-- Objects
----------------------------------------------------

xmppInterface :: PSState -> Interface
xmppInterface st = Interface
                [ addPeerMethod st
                , createIdentityMethod st
                , getContactIdentitiesMethod st
                , getContactPeersMethod st
                , getContactsMethod st
                , getCredentialsMethod st
                , getIdentitiesMethod
                , getIdentityChallengesMethod st
                , getSessionByIdentityMethod st
                , getSessionByJIDMethod st
                , getTrustStatusMethod st
                , getUnlinkedPeersMethod st
                , initializeMethod st
                , initiateChallengeMethod st
                , linkIdentityMethod st
                , linkPeersContactsMethod st
                , markKeyVerifiedMethod st
                , newContactMethod st
                , removeChallengeMethod st
                , removeContactMethod st
                , removePeerMethod st
                , renameContactMethod st
                , respondChallengeMethod st
                , revokeIdentityMethod st
                , setCredentialsMethod st
                , setIdentityMethod st
                , unlinkIdentityMethod st
                , ignorePeerMethod st
                , unignorePeerMethod st
                -- , registerAccountMethod
                -- , securityHistoryByJidMethod
                -- , securityHistoryByKeyIdMethod

                ] []
                [ SSD challengeResultSignal
                , SSD contactRemovedSignal
                , SSD contactRenamedSignal
                , SSD contactStatusChangedSignal
                , SSD identityAvailabilitySignal
                , SSD identityUnlinkedSignal
                , SSD peerTrustStatusChangedSignal
                , SSD receivedChallengeSignal
                , SSD subscriptionRequestSignal
                , SSD unlinkedIdentityAvailabilitySignal
                , SSD addPeersFailedSignal
                , SSD removePeersFailedSignal
                ]
                [ SomeProperty $ identityProp st
                ]

conObject :: PSState -> Object
conObject st = object pontariusInterface (xmppInterface st)

rootObject :: PSState -> Objects
rootObject st = root pontariusObjectPath (conObject st)
