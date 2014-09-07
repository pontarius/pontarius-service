{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Gpg where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Reader
import           DBus
import qualified DBus.Types as DBus
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified GpgMe as Gpg
import           System.Log.Logger

--import           Network.Xmpp.E2E


import           Basic
import           Persist
import           Types


mkKeyRSA :: String -> String
mkKeyRSA name = unlines $
 [ "<GnupgKeyParms format=\"internal\">"
 , "Key-Type: RSA"
 , "Key-Length: 4096"
 , "Key-Usage: sign, auth"
 , "Expire-Date: 0"
 , "Name-Real: " ++ name
 , "</GnupgKeyParms>"
 ]

pontariusKeyName :: String
pontariusKeyName = "Pontarius-Service"

newGpgKey :: IO BS.ByteString
newGpgKey = do
    ctx <- Gpg.ctxNew Nothing
    Just kid <- Gpg.genKeyFingerprint <$>
                   Gpg.genKey ctx (mkKeyRSA $ pontariusKeyName)
    return kid

revokeIdentity :: MonadIO m => KeyID -> MethodHandlerT m ()
revokeIdentity keyID = do
    let text = "" :: Text
        reason = Gpg.NoReason
    ctx <- liftIO $ Gpg.ctxNew Nothing
    keys <- liftIO $ Gpg.findKeyBy ctx True Gpg.keyFingerprint (Just keyID)
    case keys of
        [key] -> liftIO $ Gpg.revoke ctx key reason text >> return ()
        [] -> DBus.methodError $
                   MsgError{ errorName = "org.pontarius.Error.Revoke"
                           , errorText = Just "Key not found"
                           , errorBody = []
                           }
        _ -> DBus.methodError $
                   MsgError{ errorName = "org.pontarius.Error.Revoke"
                           , errorText = Just "Key not unique"
                           , errorBody = []
                           }
    return ()

setSigningGpgKey :: PSState -> ByteString -> IO Bool
setSigningGpgKey st keyFpr = do
        ctx <- Gpg.ctxNew Nothing
        keys <- Gpg.getKeys ctx True
        matches <- filterM (liftM (== Just keyFpr) . Gpg.keyFingerprint) keys
        haveKey <- case matches of
            [] -> return False
            (_:_) -> return True
        runPSM st . when haveKey $ setSigningKey "gpg" keyFpr
        return haveKey


getSigningPgpKey :: PSState -> DBus.MethodHandlerT IO KeyID
getSigningPgpKey st = do
    pIdent <- (runPSM st $ getSigningKey) >>= \case
        Just pi -> return pi
        Nothing -> do
            DBus.methodError $
                   MsgError{ errorName = "org.pontarius.Error.Sign"
                           , errorText = Just "No signing key found"
                           , errorBody = []
                           }
    case privIdentKeyBackend pIdent of
        "gpg" -> return $ privIdentKeyID pIdent
        backend -> DBus.methodError $
             MsgError { errorName = "org.pontarius.Error.Sign"
                      , errorText = Just $ "Unknown key backend " <> backend
                      , errorBody = []
                      }

identityProp :: PSState -> Property ('TypeArray ('DBusSimpleType 'TypeByte))
identityProp st =
    mkProperty pontariusObjectPath pontariusInterface "Identity"
    (Just $ getSigningPgpKey st) Nothing
    PECSTrue

--setSigningKey st keyFpr

getIdentities :: IO [KeyID]
getIdentities = do
    ctx <- Gpg.ctxNew Nothing
    keys <- Gpg.getKeys ctx True
    catMaybes <$> mapM Gpg.keyFingerprint keys

-- |  Get all available private keys
getIdentitiesMethod :: Method
getIdentitiesMethod  =
    DBus.Method
    (DBus.repMethod $ (getIdentities :: IO [KeyID] ))
    "getIdentities"
    Result
    ("identities" -- ^ List of keyIDs
     :> ResultDone)

importKey :: MonadIO m => t -> ByteString -> PSM m [ByteString]
importKey _peer key = do
    ctx <- liftIO $ Gpg.ctxNew Nothing
    importResults <- liftIO $ Gpg.importKeys ctx key
    liftM catMaybes $ forM importResults $ \res ->
        case Gpg.isResult res of
            Nothing -> do
                let fPrint = Gpg.isFprint res
--                addPeerKey st peer (PubKey "gpg" fPrint)
                return $ Just fPrint
            Just err -> do
                liftIO . errorM "Pontarius.Xmpp" $ "error while importing key" ++ show err
                return Nothing

-- doGetPeerKeys st peer = do
--     mbks <- getPeerKeys st peer
--     case mbks of
--         Nothing -> Ex.throw $ DBus.MsgError "No such peer" Nothing []
--         Just ks -> return $ Set.toList ks

-- getPeerKeysMethod st =
--     DBus.Method (DBus.repMethod $ doGetPeerKeys st)
--                 "getPeerKeys"
--                 ("peer" :-> Result "keys")

-- getKeysMethod = DBus.Method (DBus.repMethod $ getKeys)
--                             "getGpgKeys"
--                             (Result "keys")

exportSigningGpgKey :: PSState -> IO (Maybe ByteString)
exportSigningGpgKey st = do
    mbKey <- runPSM st getSigningKey
    case mbKey of
        Just key | privIdentKeyBackend key == "gpg" -> do
            let kid = privIdentKeyID key
            ctx <- Gpg.ctxNew Nothing
            keys <- Gpg.getKeys ctx True
            candidates <- filterM (\k -> (== Just kid) <$> Gpg.keyFingerprint k) keys
            case candidates of
                (k:_) -> Just <$> Gpg.exportKeys ctx [k]
                _ -> return Nothing
        _ -> return Nothing

-- checkSigningKey st = do
--     (kt, kid) <- getSigningKey st
--     check <- case kt of
--         "gpg" -> do
--             ks <- getKeys
--             return $ kid `elem` ks
--         _ -> return True
--     return (kt, kid, check)

-- getSigningKeyMethod st = DBus.Method (DBus.repMethod $ checkSigningKey st)
--                             "getSigningKey"
--                             (Result "keys")

-- sign :: AcidState DaemonState
--      -> BS.ByteString
--      -> IO BS.ByteString
-- sign st bs = do
--     (kt, kid) <- getSigningKey st
--     case kt of
--         "gpg" -> signGPG kid bs
--         _ -> error "unknown signing key type"

signGPG :: MonadIO m =>
           BS.ByteString
        -> BS.ByteString
        -> m BS.ByteString
signGPG kid bs = liftIO $ do
    ctx <- Gpg.ctxNew Nothing
    keys <- Gpg.getKeys ctx True
    matches <- filterM (liftM (== Just kid) . Gpg.keyFingerprint) keys
    case matches of
        [] -> error "key does not exist" -- return Nothing
        (p:_) -> do
            sig <- Gpg.sign ctx bs p Gpg.SigModeDetach
            debug$ "Signing " ++ show bs ++ " yielded " ++ show sig
            return sig

verifyGPG :: t -> ByteString -> ByteString -> IO Bool
verifyGPG _ sig txt = do
    ctx <- Gpg.ctxNew Nothing
    debugM "Pontarius.Xmpp" $
        "Verifying signature "  ++ show sig ++ " for " ++ show txt
    res <- Ex.try $ Gpg.verifyDetach ctx txt sig
    case res of
        Left (e :: Gpg.Error) -> do
            errorM "Pontarius.Xmpp"
                $ "Verifying signature threw exception" ++ show e
            return False
        Right res | all (goodStat . Gpg.status) res -> do
            infoM "Pontarius.Xmpp" "Signature seems good."
            return True
                  | otherwise -> do
            warningM "Pontarius.Xmpp" $ "Signature problem: " ++ show res
            return False
  where
    goodStat Gpg.SigStatGood = True
    goodStat _ = False
-- ----------------------------------------
-- -- pubkeys -----------------------------
-- ----------------------------------------

-- mkError e = do
--     errorM "Pontarius.Xmpp" e
--     Ex.throw $ DBus.MsgError "org.freedesktop.DBus.Error.Failed"
--                              (Just $ Text.pack e) []

-- xpBase64 = xpPartial decode (Text.decodeUtf8 . B64.encode)
--   where
--     decode txt = case B64.decode $ Text.encodeUtf8 txt of
--         Left e -> Left $ Text.pack e
--         Right r -> Right r


-- xpPubkey = xpRoot . xpUnliftElems $
--             xpElemNodes "{org.pontarius.core.pubkey}pubkey" $
--               xpContent xpBase64

-- xpPubkeyRequest = xpRoot . xpUnliftElems $
--                     xpElemBlank "{org.pontarius.core.pubkey}pubkey-request"

-- handlePubkey :: AcidState DaemonState -> Session -> IO ()
-- handlePubkey st sess = do
--     mbChan <- listenIQ Get "org.pontarius.core.pubkey" sess
--     case mbChan of
--         Left _ -> return ()
--         Right c -> forever $ handle c
--   where
--     handle c = do
--         ticket <- atomically c
--         debugM "Pontarius.Xmpp" "Got pubkey request"
--         case unpickle xpPubkeyRequest (iqRequestPayload $ iqRequestBody ticket) of
--             Left e -> do
--                 errorM "Pontarius.Xmpp" $
--                     "unpickle error on pubkeyHandler: " ++ ppUnpickleError e
--                 _ <- answerIQ ticket (Left $ mkStanzaError BadRequest)
--                 return ()
--             Right _ -> do
--                 k <- exportSigningGpgKey st
--                 case k of
--                     Nothing -> do
--                         errorM "Pontarius.Xmpp" "Could not export GPG key"
--                         _ <- answerIQ ticket (Left $ mkStanzaError ItemNotFound)
--                         return ()
--                     Just bs -> do
--                         _ <- answerIQ ticket (Right . Just $ pickle xpPubkey bs)
--                         return ()

-- getPubkey :: AcidState DaemonState -> Session -> Jid -> IO [BS.ByteString]
-- getPubkey st session peer = do
--     answer <- sendIQ' (Just 3000000) (Just peer) Get Nothing (pickle xpPubkeyRequest ()) session
--     case answer of
--         Left e -> mkError $ "getPubkey couldn't send message: " ++ show e
--         Right (IQResponseError e) ->
--             mkError $ "getPubkey received error: " ++ show e
--         Right (IQResponseResult (IQResult{iqResultPayload = Just pl})) ->
--                case unpickle xpPubkey pl of
--                    Left e -> mkError
--                           $ "unpickle error in getPubkey : " ++ ppUnpickleError e
--                    Right pk -> importKey st peer pk
--         _ -> mkError "getPubkey: IQ didn't return a result"

-- getPubkeyMethod :: AcidState DaemonState -> Session -> DBus.Method
-- getPubkeyMethod st session = DBus.Method (DBus.repMethod $ getPubkey st session)
--                                          "getPubkey"
--                                          ("peer" :-> Result "fingerprints")
