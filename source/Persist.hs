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
import           Persist.Schema


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

addChallenge :: MonadIO m => Xmpp.Jid
             -> SessionID
             -> KeyID
             -> Bool
             -> Maybe Text
             -> PSM m ()
addChallenge peer sid kid isOut mbQuestion = do
    now <- liftIO $ getCurrentTime
    challengeID <- liftIO $ UUID.nextRandom
    _ <- runDB . insert $ Challenge challengeID peer sid kid isOut now
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

addContactJid :: MonadIO m => UUID -> Xmpp.Jid -> (PSM m) (Either Text ())
addContactJid contactID jid = runDB $ runExceptT $ do
    mbContact <- lift $ getBy (UniqueContact contactID)
    case mbContact of
        Nothing -> throwError ("Contact not found" :: Text)
        Just c -> void . lift $ upsert (ContactJid (entityKey c) jid) []
    return ()

removeContactJid :: MonadIO m => Xmpp.Jid -> PSM m ()
removeContactJid jid = runDB $ do
    deleteBy $ UniqueContactJid jid

setContactIdentity :: MonadIO m =>
                      UUID
                   -> KeyID
                   -> PSM m (Either Text (Entity ContactIdentity))
setContactIdentity contactID ident = runDB $ runExceptT $  do
    mbContact <- lift $ getBy (UniqueContact contactID)
    case mbContact of
        Nothing -> throwError ("Contact not found" :: Text)
        Just c -> lift $ upsert (ContactIdentity (entityKey c) ident) []

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
        Just ct -> get (contactIdentityContact $ entityVal ct)

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
     Just contact -> do
         map (contactIdentityIdentity . entityVal)
             <$> selectList [ContactIdentityContact ==. entityKey contact] []

getContactJids :: MonadIO m => UUID -> PSM m [Xmpp.Jid]
getContactJids c = runDB $ do
    mbContact <- getBy $ UniqueContact c
    case mbContact of
     Nothing -> do
         $logWarn $ Text.pack $ "Contact " ++ show c ++ " does not exist"
         return []
     Just contact -> do
         map (contactJidJid . entityVal)
             <$> selectList [ContactJidContact ==. entityKey contact] []

deleteContact :: MonadIO m => UUID -> PSM m (Maybe [KeyID])
deleteContact c = runDB $ do
    mbContact <- getBy $ UniqueContact c
    case mbContact of
     Nothing -> do
         $logWarn $ Text.pack $ "Contact " ++ show c ++ " does not exist"
         return Nothing
     Just (Entity cKey _cVal) -> do
         deleteWhere [ContactJidContact ==. cKey]
         ids <- selectList [ContactIdentityContact ==. cKey] []
         deleteWhere [ContactIdentityContact ==. cKey]
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
    mbP <- getBy (UniqueIgnoredPeer jid)
    return $ case mbP of
              Nothing -> False
              Just _ -> True

ignorePeer :: Xmpp.Jid -> PSM IO ()
ignorePeer jid = runDB $ do
    _ <- upsert (IgnoredPeer jid) []
    return ()

unignorePeer :: Xmpp.Jid -> PSM IO ()
unignorePeer jid = runDB $ do
    deleteWhere [IgnoredPeerJid ==. jid]
    return ()

unlinkJid :: MonadIO m => Xmpp.Jid -> ReaderT SqlBackend m ()
unlinkJid jid = do
    deleteWhere [ContactJidJid ==. jid]

addContactsJids :: [(Xmpp.Jid, Maybe UUID)] -> PSM IO ()
addContactsJids contactJids = do
    forM_ contactJids $ \(jid, mbContact) -> do
        case mbContact of
         Nothing -> ignorePeer jid
         Just cnt -> do
             contact <- getByNotFound "contact" cnt (UniqueContact cnt)
             void . runDB . insert $
                           ContactJid { contactJidContact = entityKey contact
                                      , contactJidJid = jid
                                      }
