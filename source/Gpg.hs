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
import qualified Data.Text.Encoding as Text
import qualified GpgMe as Gpg
import qualified Network.Xmpp as Xmpp
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

fromKeyID :: KeyID -> BS.ByteString
fromKeyID = Text.encodeUtf8

toKeyID :: BS.ByteString -> KeyID
toKeyID = Text.decodeUtf8

-- revokeIdentity :: MonadIO m => KeyID -> MethodHandlerT m ()
-- revokeIdentity keyID = do
--     let text = "" :: Text
--         reason = Gpg.NoReason
--     ctx <- liftIO $ Gpg.ctxNew Nothing
--     keys <- liftIO $ Gpg.findKeyBy ctx True Gpg.keyFingerprint
--               (Just $ fromKeyID keyID)
--     case keys of
--         [key] -> liftIO $ Gpg.revoke ctx key reason text >> return ()
--         [] -> DBus.methodError $
--                    MsgError{ errorName = "org.pontarius.Error.Revoke"
--                            , errorText = Just "Key not found"
--                            , errorBody = []
--                            }
--         _ -> DBus.methodError $
--                    MsgError{ errorName = "org.pontarius.Error.Revoke"
--                            , errorText = Just "Key not unique"
--                            , errorBody = []
--                            }
--     return ()

setSigningGpgKey :: PSState -> KeyID -> IO Bool
setSigningGpgKey st keyID = do
    let keyFpr = fromKeyID keyID
    ctx <- Gpg.ctxNew Nothing
    keys <- Gpg.getKeys ctx True
    matches <- filterM (liftM (== Just keyFpr) . Gpg.keyFingerprint) keys
    haveKey <- case matches of
        [] -> return False
        (_:_) -> return True
    runPSM st . when haveKey $ setSigningKey "gpg" (toKeyID keyFpr)
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

identityProp :: PSState -> Property ('DBusSimpleType 'TypeString)
identityProp st =
    mkProperty pontariusObjectPath pontariusInterface "Identity"
    (Just $ getSigningPgpKey st) Nothing
    PECSTrue

--setSigningKey st keyFpr

getIdentities :: IO [KeyID]
getIdentities = do
    ctx <- Gpg.ctxNew Nothing
    keys <- Gpg.getKeys ctx True
    map toKeyID . catMaybes <$> mapM Gpg.keyFingerprint keys

-- |  Get all available private keys
getIdentitiesMethod :: Method
getIdentitiesMethod  =
    DBus.Method
    (DBus.repMethod $ (getIdentities :: IO [KeyID] ))
    "getIdentities"
    Done
    ("identities" -- ^ List of keyIDs
     :> Done)

importKey :: MonadIO m => ByteString -> PSM m [ByteString]
importKey key = do
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

exportSigningGpgKey :: PSState -> IO (Maybe ByteString)
exportSigningGpgKey st = do
    mbKey <- runPSM st getSigningKey
    case mbKey of
        Just key | privIdentKeyBackend key == "gpg" -> do
            let kid = fromKeyID $ privIdentKeyID key
            ctx <- Gpg.ctxNew Nothing
            keys <- Gpg.getKeys ctx True
            candidates <- filterM (\k -> (== Just kid) <$> Gpg.keyFingerprint k) keys
            case candidates of
                (k:_) -> Just <$> Gpg.exportKeys ctx [k]
                _ -> return Nothing
        _ -> return Nothing

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
            logDebug $ "Signing " ++ show bs ++ " yielded " ++ show sig
            return sig

gpgGuard :: (MonadIO m, MonadPlus m) => String -> Bool -> m ()
gpgGuard reason p = case p of
    True -> return ()
    False -> liftIO (errorM "Pontarius.Xmpp" reason) >> mzero

verifyGPG :: ByteString
          -> ByteString
          -> ByteString
          -> IO Bool
verifyGPG kid sig txt = do
    ctx <- Gpg.ctxNew Nothing
    logDebug $ "Verifying signature "  ++ show sig ++ " for " ++ show txt
    res <- Ex.try $ Gpg.verifyDetach ctx txt sig -- Gpg.Error
    case res of
        Left (e :: Gpg.Error) -> do
            errorM "Pontarius.Xmpp"
                $ "Verifying signature threw exception" ++ show e
            return False
        Right [st] -> do
            gpgGuard ("could not verify signature: " ++ show st)
                (goodStat $ Gpg.status st)
            gpgGuard ("Fingerpringt doesn't match: " ++ show kid
                       ++ " /= " ++ show (Gpg.fingerprint st))
                     (Gpg.fingerprint st == kid)
            debugM "Pontarius.Xmpp" $ "Signature seems good"
            return True
        Right [] -> do
            errorM "Pontarius.Xmpp" "verifyGPG: Could not import pubkey"
            return False
        Right _ -> do
            debugM "Pontarius.Xmpp" "multiple signature results"
            return False

  where
    goodStat Gpg.SigStatGood = True
    goodStat _ = False

importIdent :: MonadIO m => BS.ByteString -> PSM m [BS.ByteString]
importIdent ident = do
    ids <- importKey ident
    forM_ ids $ addPubIdent . toKeyID
    return ids

verifySignature :: MonadIO m =>
                   PSState
                -> Xmpp.Jid
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> m (Maybe BS.ByteString)
verifySignature st _peer pk sig pt = runPSM st $ do
    ids <- importIdent pk
    case ids of
        [id] -> do
            verified <- liftIO $ verifyGPG id sig pt
            logDebug $ "Signature is: "  ++ show verified
            return $ if verified then (Just id) else Nothing
        _ -> do
            logDebug "import resulted in more than one key"
            return Nothing
