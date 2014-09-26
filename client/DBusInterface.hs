{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module DBusInterface where

import           DBus
import           DBus.Scaffold
import           Data.Char
import           Data.Monoid

import           Control.Monad
import           Control.Monad.IO.Class
import           DBus
import           DBus.Types
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
        sigDs = nodeSignals "" node
        downcase [] = []
        downcase (x:xs) = toLower x : xs
    mfs <- forM methods
             $ methodFunction (downcase . Text.unpack . methodMember)
             (Just "org.pontarius")
    props <- forM propDs $ propertyFromDescription
             (downcase . (++ "P") . Text.unpack . pdName)
             (Just "org.pontarius")
    sigs <- forM sigDs $ \(ssd@(SSD sd)) ->
        liftSignalD (downcase . Text.unpack $ signalDMember sd <> "Signal") ssd
    return . concat $ mfs ++ props ++ sigs

 )
