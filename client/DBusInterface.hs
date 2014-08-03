{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module DBusInterface where

import           DBus
import           DBus.Scaffold
import           Data.Char

import           Control.Monad
import           Control.Monad.IO.Class
import           DBus
import           DBus.Introspect
import           DBus.Scaffold
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text

$(do
    node <- readIntrospectXml "dbus-interface.xml"
    let methods = nodeMethodDescriptions "" node
        propDs = nodePropertyDescriptions "" node
        downcase [] = []
        downcase (x:xs) = toLower x : xs
    mfs <- forM methods
             $ methodFunction (Text.unpack . methodMember)
             (Just "pontarius.service")
    props <- forM propDs $ propertyFromDescription
             (downcase . (++ "P") . Text.unpack . pdName)
             (Just "pontarius.service")
    return . concat $ mfs ++ props

 )
