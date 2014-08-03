{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module GUI where

import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Map (Map)
import qualified Data.Map as Map
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.WidgetBuilder
import           Graphics.UI.Prototyper

keymap = Map.empty

labeledInput :: BoxClass t => String -> ReaderT t IO Entry
labeledInput labelText = fmap fst . packNatural . withHBoxNew $ do
    label <- packNatural $ addLabel (Just labelText)
    entry <- packGrow addEntry
    return entry

settingsPage :: ReaderT WidgetAdder IO (Entry, Entry, Entry)
settingsPage = fmap (fst . fst) . withFrame (Just "credentials")
               . withVBoxNew $ do
    host <- labeledInput "host"
    username <- labeledInput "username"
    password <- labeledInput "password"
    return (host, username, password)

mkMainView = fmap (toWidget . snd) . withNotebook $ do
    addPage "settings" settingsPage

main = uiMain mkMainView keymap (return ())
