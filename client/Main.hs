{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Message
import           DBus.MessageBus
import           DBus.Types
import qualified DBus.Property as DBus
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text as Text
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.WidgetBuilder
import           Graphics.UI.Prototyper
import           Network.Xmpp (jid, Jid)
import           System.Environment
import           System.Exit
import           System.IO

import           DBusInterface

keymap = Map.empty

labeledInput labelText = fmap fst . packNatural . withHBoxNew $ do
    label <- packNatural $ addLabel (Just labelText)
    entry <- packGrow addEntry
    return entry

textProperty label property con = packNatural . withHBoxNew $ do
    input <- labeledInput label
    setButton <- packNatural . addButton "set" $ do
        val <- Text.pack <$> entryGetText input
        DBus.setProperty property val con
        return ()
    getButton <- packNatural . addButton "get" $ do
        mbVal <- DBus.getProperty property con
        case mbVal of
            Left e -> putStrLn "Error getting roperty"
            Right v -> entrySetText input (Text.unpack v)
    return ()

settingsPage con = fmap (fst . fst) . withFrame (Just "credentials")
               . withVBoxNew $ do
    hostEntry <- textProperty "host" hostnameP con
    usernameEntry <- textProperty "username" usernameP con
    passwordEntry <- textProperty "password" passwordP con

    -- packNatural $ withHButtonBoxNew $ do
    --     button <- packNatural . addButton "set" $ do
    --         host     <- Text.pack <$> entryGetText hostEntry
    --         username <- Text.pack <$> entryGetText usernameEntry
    --         password <- Text.pack <$> entryGetText passwordEntry
    --         DBus.setProperty hostP host con
    --         DBus.setProperty usernameP username con
    --         DBus.setProperty passwordP password con
    --         return ()
    --     button <- packNatural . addButton "get" $ do
    --         DBus.getProperty hostP
    --         host     <- Text.pack <$> entryGetText hostEntry
    --         username <- Text.pack <$> entryGetText usernameEntry
    --         password <- Text.pack <$> entryGetText passwordEntry
    --         DBus.setProperty hostP host con
    --         return ()
    return ()

mkMainView con = fmap (toWidget . snd) . withNotebook $ do
    addPage "settings" (settingsPage con)

main = do
    con <- connectBus Session (\_ _ _ -> return ()) (\_ _ _ -> return ())
    uiMain (mkMainView con) keymap (return ())
