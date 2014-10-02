{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Foldable as Foldable
import           Data.Text (Text)
import           Data.Time.Clock
import           Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Network.Xmpp as Xmpp
import           Persist.Schema

import           Types


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
    _ <- runDB $ insertUnique (PubIdent "gpg" keyID Nothing Nothing now (Just now))
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

revokeIdentity :: MonadIO m => KeyID -> ReaderT SqlBackend m ()
revokeIdentity keyID = do
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
