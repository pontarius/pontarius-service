{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs#-}
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
import           DBus.Signal
import           DBus.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Data
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
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
import           System.Log.Logger

import           DBusInterface hiding (set)
import           Types

keymap = Map.empty

data Timeout = Timeout deriving (Show, Eq, Typeable)

instance Ex.Exception Timeout

timedButton :: Text -> Integer -> ReaderT ReaderState IO a -> MkGUI c Button
timedButton name t m = addButton name $ do
    st <- ask
    res <- liftIO . Ex.try . timeout t $ runReaderT m st
    case res of
        Left e -> logToWindow $ "DBusError: " ++ show (e :: MethodError)
        Right Nothing -> logToWindow "Button press resulted in timeout."
        (Right (Just _)) -> return ()

performer  :: (ContainerClass c) =>
              Text
           -> (Text -> ReaderT ReaderState IO a)
           -> MkGUI c ((Button, HBox), Frame)
performer name m = withFrame (Just name) $ withHBoxNew $ do
    ent <- packGrow addEntry
    timedButton "execute" 250000 $ do
        txt <- liftIO $ entryGetText ent
        m txt


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
    usernameInput <- labeledInput "username"
    passwordInput <- labeledInput "password"
    withHBoxNew $ do
        timedButton "send" 250000 . liftIO $ do
            username <- entryGetText usernameInput :: IO Text
            password <- entryGetText passwordInput :: IO Text
            (throwME $ setCredentials username password dbuscon) :: IO ()
            return ()
        timedButton "get" 250000 $ do
            res <- liftIO $ getCredentials dbuscon
            case res of
                Left _ -> return ()
                Right username -> do
                    logToWindow $ "Got credentials " ++ (Text.unpack username)
                    liftIO $ do entrySetText usernameInput (username :: Text)
    return ()

identity dbusCon = withVBoxNew $ do
    timedButton "new identity" 250000 $ do
        ident <- liftIO (throwME $ createIdentity dbusCon
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

connectionPage con =  withVBoxNew $ do
    withFrame (Just "credentials") $ credentials con
    withFrame (Just "identity") $ identity con
    withFrame (Just "actions") $ withHBoxNew$  do
        timedButton "initialize" 250000 $ do
            res <- liftIO $ (initialize con
                             :: IO (Either MethodError PontariusState))
            logToWindow $ show res
        timedButton "enable" 250000 . liftIO $
            (throwME $ DBus.setProperty accountEnabledP True con :: IO ())
        timedButton "disable" 250000 . liftIO $
            (throwME $ DBus.setProperty accountEnabledP False con :: IO ())

identitiesPage con = withVBoxNew $ do
    (siLabel, _) <- withHBoxNew $ do
        addLabel (Just "selected identity")
        addLabel (Just "<identity>")
    idWindow <- packGrow $ withListWindow (do
        keyC <- addColumn "key" [ textRenderer (return . show) ]
        lift $ set keyC [treeViewColumnExpand := True])
                         (\id -> liftIO (throwME (setIdentity id con) :: IO ()))
    timedButton "getKeys" 500000 . liftIO $ do
        ids <- (throwME $ getIdentities con) :: IO [Text]
        setListWindow idWindow ids
        id <-  DBus.getProperty identityP con
                    :: IO (Either MethodError Text)
        labelSetText siLabel (either (const $ "no identity set") show id)

listView colName action =
    withListWindow
        (do col <- addColumn colName [ textRenderer (return . show) ]
            lift $ set col [treeViewColumnExpand := True])
        action

peersPage con = withVBoxNew $ do
    peersWindow <- packGrow $ listView "peer" (\id -> return ())
    timedButton "get peers" 250000 $ do
        ps <- liftIO (throwME $ DBus.getProperty peersP con
                      :: IO [(Text, Bool)])
        liftIO $ setListWindow peersWindow ps
    withHBoxNew $ do
        performer "add peer" $ \peer ->
            liftIO (throwME $ addPeer peer con :: IO ())
        timedButton "remove selected peers" 250000 . liftIO $ do
            peers <- listWindowGetSelectedItems peersWindow
            forM_ peers $ \(peer,_) -> (throwME $ removePeer peer con :: IO ())
    avPeersWindow <- packGrow $ listView "peer"
                     (\id -> do
                           res <- liftIO (startAKE id con
                                          :: IO (Either MethodError Bool))
                           logToWindow $ show res)
    timedButton "get available peers" 250000 $ do
        ps <- liftIO (throwME $ DBus.getProperty availableEntitiesP con :: IO [Text])
        liftIO $ setListWindow avPeersWindow ps
    peerStatus <- labeledInput "peerStatus signal"
    liftIO $ addSignalHandler anySignal{matchMember = Just "peerStatusChanged"}
        mempty
        (\sg -> postGUIAsync $ entrySetText peerStatus (show (signalBody sg)))
        con
    challengerField <- labeledInput "challenger"
    questionField <- labeledInput "challenge question"
    secretField <- labeledInput "secret"
    liftIO $ addSignalHandler anySignal{matchMember = Just "receivedChallenge"}
        mempty
        (\sg -> case signalBody sg of
                 [  DBV (DBVString chalPeer)
                  , DBV (DBVString quest)] -> postGUIAsync $ do
                     entrySetText challengerField chalPeer
                     entrySetText questionField quest
                 xs -> putStrLn $ "challenge signal type error" ++ show xs
        )
        con
    timedButton "challenge" 250000 $ do
        peers <- liftIO $ listWindowGetSelectedItems avPeersWindow
        liftIO . forM_ peers $ \peer -> do
            question <- entryGetText questionField :: IO Text
            secret <- entryGetText secretField :: IO Text
            throwME (initiateChallenge (peer :: Text) question secret con) :: IO ()
    timedButton "respond challenge" 250000 $ liftIO $ do
        peer <- entryGetText challengerField :: IO Text
        secret <- entryGetText secretField :: IO Text
        throwME (respondChallenge peer secret con) :: IO ()
    (trustLabel, _) <- withHBoxNew $ do
        addLabel (Just "trust status") >> addLabel Nothing
    liftIO $ addSignalHandler anySignal{matchMember = Just "peerTrustStatusChanged"}
        mempty
        (\sg -> case signalBody sg of
                 [  DBV (DBVString peer)
                  , DBV (DBVString status)] -> postGUIAsync $ do
                     labelSetText trustLabel $ peer <> " / " <> status
                 xs -> putStrLn $ "challenge signal type error" ++ show xs
        )
        con
    return peersWindow


challengesPage peers con = withVBoxNew $ do
    challengesWindow <- listView "challenges" (\id -> return ())
    timedButton "getChallenges" 250000 $ liftIO $ do
        ps <- listWindowGetSelectedItems peers
        case ps of
             [] -> return ()
             [(p, _)] -> do
                 chals <- (throwME (getChallenges p con)
                              :: IO [(Text, Text, Bool, Text, Text, Text, Bool)])
                 setListWindow challengesWindow chals

mkMainView signalChan con = fmap (toWidget . snd) . withNotebook $ do
    addPage "connection" $ connectionPage con
    addPage "identities" $ identitiesPage con
    (identView, _) <- addPage "peers" $ peersPage con
    addPage "challenges" $ challengesPage identView con

main = do
    updateGlobalLogger "DBus" $ setLevel DEBUG
    sChan <- newTChanIO
    con <- connectBus Session (\_ _ _ -> return ())
               (\x y z -> atomically $ writeTChan sChan (x,y,z))
    putStrLn "dbus connected"
    -- let matchPropertiesSignal =
    --         MatchSignal{ matchInterface = Just "org.freedesktop.DBus.Properties"
    --                    , matchMember = Nothing
    --                    , matchPath = Nothing
    --                    , matchSender = Just "org.pontarius"
    --                    }
    -- addSignalHandler matchPropertiesSignal mempty print con
    uiMain (mkMainView sChan con) keymap (return ())
    return signalChan
