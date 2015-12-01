{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Persist.Stage where

import           Control.Lens
import           Control.Monad
import qualified Data.Aeson as Aeson
import           Data.Char
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid
import           Data.UUID
import qualified Data.UUID as UUID
import           Database.Persist.Sql
import           Language.Haskell.TH
import           Network.Xmpp
import           Web.PathPieces
import           Web.HttpApiData

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

instance PersistField UUID where
    toPersistValue = toPersistValue . BS.concat . BSL.toChunks . toByteString
    fromPersistValue = \x -> fromPersistValue x >>= \v ->
        case fromByteString $ BSL.fromChunks [v] of
            Nothing -> Left $ Text.concat ["Invalid UUID: ", Text.pack (show v)]
            Just u -> Right u

instance PersistFieldSql UUID where
    sqlType _ = SqlInt64

instance PersistField Jid where
    toPersistValue = toPersistValue . Text.encodeUtf8 . jidToText
    fromPersistValue = \x -> fromPersistValue x >>= \v ->
        case jidFromText $ Text.decodeUtf8 v of
            Nothing -> Left $ Text.concat ["Invalid JID: ", Text.pack (show v)]
            Just j -> Right j

instance Aeson.ToJSON UUID where
    toJSON uuid = Aeson.toJSON $ UUID.toText uuid

instance Aeson.FromJSON UUID where
    parseJSON v = do
        txt <- Aeson.parseJSON v
        case UUID.fromText txt of
         Nothing -> fail $ "can't parse UUID" <> (Text.unpack txt)
         Just uuid -> return uuid


instance PathPiece UUID where
    fromPathPiece = UUID.fromString . Text.unpack
    toPathPiece = Text.pack . UUID.toString

instance PersistFieldSql Jid where
    sqlType _ = SqlString

instance PathPiece Jid where
    fromPathPiece = jidFromText
    toPathPiece = jidToText

instance Aeson.ToJSON Jid where
    toJSON = Aeson.toJSON . jidToText

instance Aeson.FromJSON Jid where
    parseJSON v = do
        str <- Aeson.parseJSON v
        case jidFromText str of
         Nothing -> mzero
         Just jid -> return jid

instance ToHttpApiData UUID where
    toUrlPiece = toPathPiece

instance FromHttpApiData UUID where
    parseUrlPiece txt =
        case fromPathPiece txt of
         Nothing -> Left $ "Could not parse uuid " <> txt
         Just uuid -> Right uuid

instance ToHttpApiData Jid where
    toUrlPiece = toPathPiece

instance FromHttpApiData Jid where
    parseUrlPiece txt =
        case fromPathPiece txt of
         Nothing -> Left $ "Could not parse jid " <> txt
         Just jid -> Right jid

pLensRules :: [Char] -> LensRules
pLensRules pr = lensRules & lensField
                   .~ (\_ _ f -> fmap (TopName . mkName . downcase) .
                                 maybeToList . List.stripPrefix pr . (++ "L")
                                 . nameBase $ f)
  where
    downcase [] = []
    downcase (x:xs) = toLower x : xs
