{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface where

import           DBus
import           DBus.Scaffold
import           Data.Char
import           Data.Monoid

import           Control.Monad
import           DBus
import qualified Data.Text as Text

$(do
    node <- readIntrospectXml "dbus-interface.xml"
    let methods = nodeMethodDescriptions "" node
        propDs = nodePropertyDescriptions "" node
        sigDs = nodeSignals "" node
        downcase [] = []
        downcase (x:xs) = toLower x : xs
    mfs <- forM methods $ \smd@(SMD md) ->
               liftMethodDescription (downcase . Text.unpack $ methodMember md) smd
    props <- forM propDs $ propertyFromDescription
             (downcase . (++ "P") . Text.unpack . pdName)
             (Just "org.pontarius")
    sigs <- forM sigDs $ \(ssd@(SSD sd)) ->
        liftSignalDescription (downcase . Text.unpack $ signalDMember sd <> "Signal") ssd
    return . concat $ mfs ++ props ++ sigs

 )
