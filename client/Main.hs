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
import           DBus.Reactive
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
import           FRP.Sodium
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Reactive
import           Graphics.UI.Gtk.WidgetBuilder
import           Graphics.UI.Prototyper
import           Network.Xmpp (jid, Jid)
import           System.Environment
import           System.Exit
import           System.IO
import           System.Log.Logger


import           DBusInterface hiding (set)
import           Types

peer :: Text
peer = "org.pontarius"

splitEither :: Event (Either l r)
     -> (Event l, Event r)
splitEither ev = (filterJust (lToMb <$> ev), filterJust (rToMb <$> ev))
  where
    lToMb (Left l) = Just l
    lToMb Right{} = Nothing
    rToMb (Right r) = Just r
    rToMb Left{} = Nothing

call' :: (Representable ret, Representable args) =>
         MethodDescription (FlattenRepType (RepType args))
                           (FlattenRepType (RepType ret))
      -> args
      -> DBusConnection
      -> IO (Either MethodError ret)
call' md args = call md peer args []

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

credentials dbuscon = withVBoxNew $ do
    usernameEntry <- labeledEntry "username"
    liftIO $ editableInsertText usernameEntry ("foo" :: Text) 0
    passwordEntry <- labeledEntry "password"
    usernameB <- liftIO $ inputB usernameEntry
    passwordB <- liftIO $ inputB passwordEntry
    (setE, getE) <-
        withHBoxNew_ $ (,) <$> (addButtonE "set") <*> (addButtonE "get")

    -- Reactive
    le <- liftIO . sync $ do
        let credentialsE :: Event (Text, Text)
            credentialsE = setE @> ((,) <$> usernameB <*> passwordB)
        sendE <- eventMethod setCredentials peer credentialsE dbuscon
        getE <- eventMethod getCredentials peer getE dbuscon
        let (getEl, getEr) = splitEither getE :: (Event MethodError, Event Text)
        let (sendEl, sendEr) = splitEither sendE :: (Event MethodError, Event ())
        eventSetInput usernameEntry (getEr)
        return $ (show <$> sendEl) `merge`
                 (show <$> getEl) `merge`
                 (show <$> getEr)
    logEvent le
    return ()

connectionPage con =  withVBoxNew $ do
    withFrame (Just "credentials") $ credentials con
    withFrame (Just "actions") $ withHBoxNew$  do
        timedButton "initialize" 250000 $ do
            res <- liftIO $ (call' initialize () con
                             :: IO (Either MethodError (PontariusState, Map Text (Text, [Text]), [Text])))
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
                         (\id -> liftIO (throwME (call' setIdentity id con) :: IO ()))
    timedButton "getKeys" 500000 . liftIO $ do
        ids <- (throwME $ call' getIdentities () con) :: IO [Text]
        setListWindow idWindow ids
        -- id <-  DBus.getProperty identityP con
        --             :: IO (Either MethodError Text)
        -- labelSetText siLabel (either (const $ "no identity set") show id)

-- listView colName action =
--     withListWindow
--         (do col <- addColumn colName [ textRenderer (return . show) ]
--             lift $ set col [treeViewColumnExpand := True])
--         action

-- peersPage con = withVBoxNew $ do
--     contacts

-- challengesPage peers con = withVBoxNew $ do
--     challengesWindow <- listView "challenges" (\id -> return ())
--     timedButton "getChallenges" 250000 $ liftIO $ do
--         ps <- listWindowGetSelectedItems peers
--         case ps of
--              [] -> return ()
--              [(p, _)] -> do
--                  chals <- (throwME (getChallenges p con)
--                               :: IO [(Text, Text, Bool, Text, Text, Text, Bool)])
--                  setListWindow challengesWindow chals

mkMainView signalChan con = fmap (toWidget . snd) . withNotebook $ do
    addPage "connection" $ connectionPage con
    addPage "identities" $ identitiesPage con
    cStatusE <- liftIO $ signalEvent contactStatusChangedSignal Nothing con
    logEvent (show <$> (cStatusE :: Event (Text, PeerStatus)))
    upStatusE <- liftIO $ signalEvent unlinkedIdentityStatusChangedSignal Nothing con
    logEvent (show <$> (upStatusE :: Event (Text, Jid, PeerStatus)))
    -- (identView, _) <- addPage "peers" $ peersPage con
    -- addPage "challenges" $ challengesPage identView con

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
