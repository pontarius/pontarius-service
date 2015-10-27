{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Persist
  ( module Persist
  , module Persist.Schema
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           DBus (MsgError(..))
import qualified Data.Foldable as Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import           Database.Persist
import           Database.Persist.Class ()
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp
import           Persist.Schema hiding (peerIgnored)
import qualified Persist.Schema as DB

import           Types

getByNotFound :: (Show a, MonadThrow m, MonadIO m, PersistEntity val,
                  PersistEntityBackend val ~ SqlBackend) =>
                 Text
              -> a
              -> Unique val
              -> PSM m (Entity val)
getByNotFound desc item unique = do
    res <- runDB $ getBy unique
    case res of
     Nothing ->
         throwM $
           MsgError{ errorName =  "org.pontarius.Error"
                   , errorText = Just $ mconcat
                       [ "Could not find "
                       , desc
                       , " : "
                       , Text.pack $ show item
                       ]
                   , errorBody =   []
                   }
     Just r -> return r


setCredentials :: MonadIO m => Text -> Xmpp.Username -> Xmpp.Password -> PSM m ()
setCredentials hostName username password = do
    runDB $ do
        deleteWhere ([] :: [Filter HostCredentials])
        now <- liftIO getCurrentTime
        _ <- insert $ HostCredentials hostName username password now
        return ()

getCredentials :: MonadIO m => PSM m (Maybe HostCredentials)
getCredentials = runDB $ fmap entityVal
                <$> selectFirst [] [Desc HostCredentialsChanged]

addIdentity :: MonadIO m =>
                 Text
              -> KeyID
              -> Maybe UTCTime
              -> Maybe UTCTime
              -> PSM m ()
addIdentity keyBackend keyID keyCreated keyImported = do
    sk <- getSigningKey
    let isDefault = maybe Active (const Inactive) sk
    _ <- runDB . insert $ PrivIdent keyBackend keyID Nothing keyCreated keyImported
                     isDefault
    return ()

setSigningKey :: MonadIO m => Text -> KeyID -> PSM m ()
setSigningKey backend keyID = runDB $ do
    _ <- insertUnique (PrivIdent "gpg" keyID Nothing Nothing Nothing Inactive)
    updateWhere [ PrivIdentIsDefault ==. Active] [PrivIdentIsDefault =. Inactive]
    updateWhere [ PrivIdentKeyBackend ==. backend
                , PrivIdentKeyID ==. keyID
                , PrivIdentRevoked ==. Nothing
                ]
                [ PrivIdentIsDefault =. Active]
    return ()

getSigningKey :: MonadIO m => PSM m (Maybe PrivIdent)
getSigningKey = runDB $ do
    fmap entityVal <$> getBy (UniqueDefaultKey Active)

addPubIdent :: MonadIO m => KeyID -> PSM m ()
addPubIdent keyID = do
    now <- liftIO $ getCurrentTime
    _ <- runDB $ insertUnique
                    PubIdent{ pubIdentKeyBackend = "gpg"
                            , pubIdentKeyID = keyID
                            , pubIdentVerified = Nothing
                            , pubIdentRevoked = Nothing
                            , pubIdentReceived = now
                            , pubIdentImported = (Just now)
                            }
    return ()


runDB :: MonadIO m => SqlPersistT (LoggingT (ResourceT IO)) b -> PSM m b
runDB f = do
    dbPool <- PSM $ view psDB
    PSM . liftIO . runResourceT . runStderrLoggingT . flip runSqlPool dbPool $ f

setState :: (MonadReader PSState m, MonadIO m) => PontariusState -> m ()
setState newState = do
    st <- view psState
    liftIO . atomically $ writeTVar st newState

getState :: (MonadReader PSState m, MonadIO m) => m PontariusState
getState = do
    st <- view psState
    liftIO . atomically $ readTVar st

addChallenge :: (MonadIO m, MonadThrow m) =>
                Xmpp.Jid
             -> SessionID
             -> KeyID
             -> Bool
             -> Maybe Text
             -> PSM m ()
addChallenge peer sid kid isOut mbQuestion = do
    now <- liftIO $ getCurrentTime
    challengeID <- liftIO $ UUID.nextRandom
    Entity sessionK _ <- getByNotFound "session by id" sid (UniqueSession sid)
    _ <- runDB . insert $ Challenge challengeID peer sessionK kid isOut now
                                    Nothing mbQuestion Nothing False
    return ()

setChallengeCompleted :: MonadIO m => Xmpp.Jid -> Bool -> PSM m ()
setChallengeCompleted peer trust = runDB $ do
    mbC <- selectFirst [ ChallengeCompleted ==. Nothing
                       , ChallengePeer ==. peer
                       ]
                       [Desc ChallengeStarted]
    now <- liftIO getCurrentTime
    Foldable.forM_ mbC $ \chal ->
        update (entityKey chal) [ ChallengeCompleted =. Just now
                                , ChallengeResult =. Just trust
                                ]


getPeerChallenges :: (MonadIO m, Functor m) => Xmpp.Jid -> PSM m [Challenge]
getPeerChallenges peer
    = map entityVal <$> runDB (selectList
                               [ ChallengeHidden ==. False
                               , ChallengePeer ==. (Xmpp.toBare peer)
                               ]
                               [Asc ChallengeStarted])

getIdentityChallenges :: (MonadIO m, Functor m) => KeyID -> PSM m [Challenge]
getIdentityChallenges keyID
    = map entityVal <$> runDB (selectList
                               [ ChallengeHidden ==. False
                               , ChallengeKeyID ==. keyID
                               ]
                               [Asc ChallengeStarted])



hideChallenge :: MonadIO m => UUID -> PSM m ()
hideChallenge challengeID = runDB $ do
    updateWhere [ChallengeUniqueID ==. challengeID] [ChallengeHidden =. True]
    return ()

setKeyVerified :: MonadIO m => KeyID -> Bool -> PSM m ()
setKeyVerified keyID isVerified = runDB $ do
    verifyTime <- if isVerified
                     then liftM Just $ liftIO getCurrentTime
                     else return Nothing

    updateWhere [PubIdentKeyID ==. keyID] [PubIdentVerified =. verifyTime]

isKeyVerified :: MonadIO m => KeyID -> PSM m (Maybe (Maybe UTCTime))
isKeyVerified keyID = runDB $ do
    mbKey <- getBy (UniquePubIdentKey keyID)
    return $ pubIdentVerified . entityVal <$> mbKey

revokeIdentity :: KeyID -> PSM IO ()
revokeIdentity keyID = runDB $ do
    now <- liftIO getCurrentTime
    updateWhere [PubIdentKeyID ==. keyID] [PubIdentRevoked =. Just now]

haveKey :: MonadIO m => KeyID -> PSM m Bool
haveKey kid = runDB $ do
    res <- getBy (UniquePubIdentKey kid)
    return $ maybe False (const True) res

newContact :: MonadIO m => Text -> PSM m UUID
newContact name = runDB $ do
    contactID <- liftIO UUID.nextRandom
    _ <- insert $ Contact contactID name
    return contactID

renameContact :: MonadIO m => UUID -> Text -> PSM m ()
renameContact contactID newName = runDB $ do
    updateWhere [ContactUniqueID ==. contactID] [ContactName =. newName]


-- Link peer
addContactPeer :: MonadIO m => UUID -> Xmpp.Jid -> PSM m ()
addContactPeer contactID jid =
    void . runDB $ upsert (ContactPeer jid contactID) []

-- unlink peer
removeContactPeer :: MonadIO m => Xmpp.Jid -> PSM m ()
removeContactPeer jid = runDB $ do
    deleteBy $ UniqueContactPeer jid

setContactIdentity :: MonadIO m =>
                      UUID
                   -> KeyID
                   -> PSM m ()
setContactIdentity contactID ident = do
    _ <- runDB $ upsert (ContactIdentity ident contactID) []
    return ()

removeContactIdentity :: MonadIO m => KeyID -> PSM m ()
removeContactIdentity id = runDB $ do
    deleteBy $ UniqueContactIdentity id

getContactByIdentity :: MonadIO m =>
                        KeyID
                     -> PSM m (Maybe (Entity ContactIdentity))
getContactByIdentity id =
    runDB $ getBy (UniqueContactIdentity id)

getIdentityContact :: MonadIO m => KeyID -> PSM m (Maybe Contact)
getIdentityContact keyID = runDB $ do
    mbContactId <- getBy (UniqueContactIdentity keyID)
    case mbContactId of
        Nothing -> return Nothing
        Just ct -> do
            e <- getBy (UniqueContact $ contactIdentityContact $ entityVal ct)
            return $ entityVal <$> e

getContacts :: MonadIO m => PSM m [Contact]
getContacts = runDB $ do
    map entityVal <$>  selectList []  []

getContactIds :: MonadIO m => UUID -> PSM m [KeyID]
getContactIds c = runDB $ do
    mbContact <- getBy $ UniqueContact c
    case mbContact of
     Nothing -> do
         $logWarn $ Text.pack $ "Contact " ++ show c ++ " does not exist"
         return []
     Just _ -> do
         map (contactIdentityIdentity . entityVal)
             <$> selectList [ContactIdentityContact ==. c] []

getContactPeers :: MonadIO m => UUID -> PSM m [Xmpp.Jid]
getContactPeers c = runDB $ do
    mbContact <- getBy $ UniqueContact c
    case mbContact of
     Nothing -> do
         $logWarn $ Text.pack $ "Contact " ++ show c ++ " does not exist"
         return []
     Just _ -> do
         map (contactPeerPeer . entityVal)
             <$> selectList [ContactPeerContact ==. c] []

deleteContact :: MonadIO m => UUID -> PSM m (Maybe [KeyID])
deleteContact c = runDB $ do
    mbContact <- getBy $ UniqueContact c
    case mbContact of
     Nothing -> do
         $logWarn $ Text.pack $ "Contact " ++ show c ++ " does not exist"
         return Nothing
     Just _ -> do
         deleteWhere [ContactPeerContact ==. c]
         ids <- selectList [ContactIdentityContact ==. c] []
         deleteWhere [ContactIdentityContact ==. c]
         deleteBy $ UniqueContact c
         return $ Just (contactIdentityIdentity . entityVal <$> ids)

addSession :: MonadIO m =>
              SessionID
           -> Maybe KeyID
           -> Xmpp.Jid
           -> Xmpp.Jid
           -> PSM m ()
addSession sid ident local remote = do
    sk <- fmap privIdentKeyID `liftM` getSigningKey
    now <- liftIO $ getCurrentTime
    _ <- runDB . insert $ Session sid sk ident local remote now Nothing
    return ()

concludeSession :: MonadIO m => SessionID -> PSM m ()
concludeSession sid = do
    now <- liftIO getCurrentTime
    runDB $ updateWhere [ SessionSessionID ==. sid
                        , SessionConcluded ==. Nothing
                        ] [SessionConcluded =. Just now]
    return ()

getIdentitySessions :: MonadIO m => KeyID -> PSM m [Session]
getIdentitySessions id = liftM (map entityVal) $
    runDB $ selectList [SessionPubIdentID ==. Just id] [Asc SessionInitiated]

getJidSessions :: MonadIO m => Xmpp.Jid -> PSM m [Session]
getJidSessions jid = liftM (map entityVal) $
    runDB $ selectList [SessionRemoteJid ==. jid] [Asc SessionInitiated]

peerIgnored :: MonadIO m => Xmpp.Jid -> PSM m Bool
peerIgnored jid = runDB $ do
    mbP <- getBy (UniquePeer jid)
    return $ case mbP of
              Nothing -> False
              Just (Entity _ p) -> DB.peerIgnored p

ignorePeer :: Xmpp.Jid -> PSM IO ()
ignorePeer jid = runDB $ do
    _ <- upsert (Peer jid True False False) [PeerIgnored =. True]
    return ()

unignorePeer :: Xmpp.Jid -> PSM IO ()
unignorePeer jid = runDB $ do
    updateWhere [DB.PeerJid ==. jid] [DB.PeerIgnored =. False]
    return ()

unlinkJid :: MonadIO m => Xmpp.Jid -> ReaderT SqlBackend m ()
unlinkJid jid = do
    deleteWhere [ContactPeerPeer ==. jid]

batchLink :: [(Xmpp.Jid, BatchLink)] -> PSM IO ()
batchLink ps = do
    forM_ ps $ \(peer, action) ->
        case action of
          BatchLinkIgnore -> ignorePeer peer
          BatchLinkExisting uuid -> addContactPeer uuid peer
          BatchLinkNewContact name -> do
              contact <- newContact name
              addContactPeer contact peer

getPeer :: MonadIO m => Xmpp.Jid -> PSM m (Maybe Peer)
getPeer jid = do
    res <- runDB $ getBy (UniquePeer jid)
    return $ entityVal <$> res

addPeer :: MonadIO m => Xmpp.Jid -> Bool -> PSM m ()
addPeer jid add = do
    _ <- runDB $ upsert (DB.Peer{ DB.peerJid = jid
                                , DB.peerIgnored = False
                                , DB.peerAdd = add
                                , DB.peerRemove = False
                                })
         [ DB.PeerIgnored =. False
         , DB.PeerAdd =. add
         ]
    return ()

getAddPeers :: MonadIO m => PSM m [Peer]
getAddPeers = do
    runDB $ fmap entityVal <$> selectList [DB.PeerAdd ==. True] []

getRemovePeers :: MonadIO m => PSM m [Peer]
getRemovePeers = do
    runDB $ fmap entityVal <$> selectList [DB.PeerRemove ==. True] []
