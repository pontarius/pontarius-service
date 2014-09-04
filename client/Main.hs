{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.Timeout
import qualified Control.Exception as Ex
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Message
import           DBus.MessageBus
import qualified DBus.Property as DBus
import           DBus.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Data
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text as Text
import           Data.Typeable
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.WidgetBuilder
import           Graphics.UI.Prototyper
import           Network.Xmpp (jid, Jid)
import           System.Environment
import           System.Exit
import           System.IO

import           DBusInterface
import           Types

keymap = Map.empty


data Timeout = Timeout deriving (Show, Eq, Typeable)

instance Ex.Exception Timeout

timedButton name t m = addButton name $ do
    st <- ask
    res <- liftIO . Ex.try . timeout t $ runReaderT m st
    case res of
        Left e -> logToWindow $ "DBusError: " ++ show (e :: MethodError)
        Right Nothing -> logToWindow "Button press resulted in timeout."
        (Right (Just _)) -> return ()

throwME :: IO (Either MethodError a) -> IO a
throwME m = do
    res <- m
    case res of
        Left e -> Ex.throwIO e
        Right r -> return r

labeledInput labelText = fmap fst . packNatural . withHBoxNew $ do
    label <- packNatural $ addLabel (Just labelText)
    entry <- packGrow addEntry
    return entry

credentials dbuscon = withVBoxNew $ do
    hostInput <- labeledInput "host"
    usernameInput <- labeledInput "username"
    passwordInput <- labeledInput "password"
    withHBoxNew $ do
        timedButton "send" 250000 . liftIO $ do
            host <- entryGetText hostInput :: IO Text
            username <- entryGetText usernameInput :: IO Text
            password <- entryGetText passwordInput :: IO Text
            (throwME $ set_credentials host username password dbuscon) :: IO ()
            return ()
        timedButton "get" 250000 $ do
            res <- liftIO $ get_credentials dbuscon
            case res of
                Left _ -> return ()
                Right (host, username) -> do
                    logToWindow $ "Got credentials " ++ (Text.unpack host)
                                   ++ " / " ++ (Text.unpack username)
                    liftIO $ do
                        entrySetText hostInput (host :: Text)
                        entrySetText usernameInput (username :: Text)
    return ()

identity dbusCon = withVBoxNew $ do
    identiInput <- labeledInput "identity"
    timedButton "new identity" 250000 $ do
        identName <- liftIO $ (entryGetText identiInput :: IO Text )
        ident <- liftIO (throwME $ createIdentity identName dbusCon
                     :: IO ByteString)
        logToWindow $ "Identity created: " ++ show ident
        return ()

textProperty label property con = withHBoxNew $ do
    input <- labeledInput label
    setButton <- timedButton "set" 250000 . liftIO $ do
        val <- Text.pack <$> entryGetText input
        DBus.setProperty property val con
        return ()
    getButton <- timedButton "get" 250000 $ do
        mbVal <- liftIO $ DBus.getProperty property con
        case mbVal of
            Left e -> logToWindow "Error getting property"
            Right v -> liftIO $ entrySetText input (Text.unpack v)
    return ()

settingsPage con =  withVBoxNew $ do
    withFrame (Just "credentials") $ credentials con
    withFrame (Just "identity") $ identity con

actionsPage con = do
    timedButton "initialize" 250000 $ do
        res <- liftIO $ (initialize con :: IO (Either MethodError PontariusState))
        logToWindow $ show res

mkMainView signalChan con = fmap (toWidget . snd) . withNotebook $ do
    addPage "settings" (settingsPage con)
    addPage "actions" $ actionsPage con

main = do
    signalChan <- newTChanIO
    con <- connectBus Session (\_ _ _ -> return ())
               (\x y z -> atomically $ writeTChan signalChan (x,y,z))
    uiMain (mkMainView signalChan con) keymap (return ())
    return signalChan
