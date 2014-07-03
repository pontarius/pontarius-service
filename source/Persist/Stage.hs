{-# LANGUAGE OverloadedStrings #-}

module Persist.Stage where

import           Control.Lens
import           Data.Char
import qualified Data.List as List
import           Data.UUID
import           Database.Persist.Sql
import           Network.Xmpp

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

instance PersistFieldSql Jid where
    sqlType _ = SqlString

pLensRules :: [Char] -> LensRules
pLensRules pr = lensRules & lensField
                   .~ (fmap downcase . List.stripPrefix pr . (++ "L"))
  where
    downcase [] = []
    downcase (x:xs) = toLower x : xs
