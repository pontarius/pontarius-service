{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Lens.TH
import qualified Control.Monad.Catch as Ex
import           Control.Monad.Reader
import qualified DBus
import           DBus hiding (call)
import           DBus.Property
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Interface
import           System.Environment
import           System.Exit
import           System.IO
import           System.Log.Logger
import           System.Timeout

import           PontariusService.Types

import           Interface

newtype JID = JID { unJID :: Text}
              deriving (Eq, Ord, Show)

instance Representable JID where
    type RepType JID = RepType Text
    toRep = toRep . unJID
    fromRep = fmap JID . fromRep

data ClientGlobals =
    ClientState { clientGlobalsConnection         :: !DBusConnection
                , clientGlobalsPropState          :: !(TVar PontariusState)
                , clientGlobalsPropAccountEnabled :: !(TVar Bool)
                , clientGlobalsPropPeers          :: !(TVar [(Text, Bool)])
                , clientGlobalsCredentials        :: !(Text, Text)
                , clientGlobalsThem               :: !Text
                }

makeLensesWith camelCaseFields ''ClientGlobals

newtype Client a = Client { unClient :: ReaderT ClientGlobals IO a}
                     deriving (Monad, MonadIO, Applicative, Functor
                              , Ex.MonadThrow, Ex.MonadCatch)


data SomeShowTVar where
    SSTV :: Show a => TVar a -> SomeShowTVar

dumpState = do
    debug "Dumping State"
    globals <- Client ask
    mapM (\(name, getter) -> do
               SSTV tv <- return $ globals ^. getter
               v <- liftIO . atomically $ readTVar tv
               debug $ " - " ++ name ++ ": " ++ show v)
        [ ("pontarius state", propState . shw)
        , ("account enabled", propAccountEnabled .shw)
        , ("peers", propPeers . shw)
        ]
  where shw = to SSTV

withTimeout :: String -> Client a -> Client a
withTimeout message (Client f) = do
    st <- Client ask
    res <- liftIO $ timeout (5*10^6) (runReaderT f st)
    case res of
     Nothing -> do
         debug $ "Timeout: " ++ message
         dumpState
         liftIO $ exitFailure
     Just r -> return r
-- withTimeout = id


getConnection :: Client DBusConnection
getConnection = Client $ view connection

getXMPPCredentials :: Client (Text, Text)
getXMPPCredentials = Client $ view credentials

getPeer = Client $ view them

peer :: Text
peer = "org.pontarius"

credentialsA :: (Text, Text)
credentialsA = ("testuser1@test.pontarius.org", "pwd1")

credentialsB :: (Text, Text)
credentialsB = ("testuser2@test.pontarius.org", "pwd2")



throwME :: Ex.Exception e => IO (Either e b) -> IO b
throwME f = do
    x <- f
    case x of
     Left e -> Ex.throwM e
     Right r -> return r

call :: (Representable args, Representable ret) =>
        MethodDescription
        (FlattenRepType (RepType args)) (FlattenRepType (RepType ret))
     -> args
     -> Client ret
call desc args = do
    con <- getConnection
    liftIO . throwME $ DBus.call desc peer args [] con

onSignal :: Representable t =>
            SignalDescription (FlattenRepType (RepType t))
         -> (t -> Client ())
         -> Client ()
onSignal sd f = do
    globals <- Client ask
    con <- getConnection
    liftIO $ DBus.handleSignal sd Nothing mempty (f' globals) con
  where
    f' globals x = runReaderT (unClient $ f x) globals

setProp :: Representable a => RemoteProperty (RepType a) -> a -> Client ()
setProp p v = do
    con <- getConnection
    () <- liftIO . throwME $ DBus.setProperty p v con
    return ()

waitProp :: String
         -> Getting (TVar t) ClientGlobals (TVar t)
         -> (t -> Bool)
         -> Client ()
waitProp message pr p = do
    tv <- Client $ view pr
    withTimeout message . liftIO . atomically $ do
      v <- readTVar tv
      unless (p v) retry
      return ()


waitForServer con = do
    liftIO $ putStrLn "Waiting for pontarius-service"
    r <- DBus.call initialize peer () [] con :: IO (Either MethodError PontariusState)
    case r of
     Left e -> do
         liftIO $ threadDelay 100000 -- 100ms
         waitForServer con
     Right _ -> return ()

debug :: String -> Client ()
debug = liftIO . putStrLn

runClient :: Client b -> IO b
runClient (Client f) = do
    args <- liftIO getArgs
    (credentials, them) <- case args of
        ["active"] -> return (credentialsA, fst credentialsB)
        ["passive"] -> return (credentialsB, fst credentialsA)
        [] -> liftIO $ do
            hPutStrLn stderr "Usage: testclient [active|passive]"
            exitFailure
    updateGlobalLogger "DBus.Reactive" $ setLevel DEBUG
    updateGlobalLogger "DBus" $ setLevel DEBUG
    con <- connectBus Session ignore ignore
    putStrLn "dbus connected"
    waitForServer con
    statusVar <- propertyToTVar statusP con
    peersVar <- propertyToTVar peersP con
    enabledVar <- propertyToTVar accountEnabledP con
    runReaderT f $ ClientState { clientGlobalsConnection = con
                               , clientGlobalsPropState = statusVar
                               , clientGlobalsPropAccountEnabled = enabledVar
                               , clientGlobalsPropPeers = peersVar
                               , clientGlobalsCredentials = credentials
                               , clientGlobalsThem = them
                               }

setup :: Client ()
setup = do
    creds <- getXMPPCredentials
    () <- call setCredentials creds
    debug "credentials set"
    idents <- call getIdentities () :: Client [Text]
    case idents of
     [] -> do
         _ <- call createIdentity () :: Client ByteString
         return ()
     (ident:_) -> call setIdentity ident
    debug "identity set"
    setProp accountEnabledP True
    debug "Waiting for XMPP connection to complete"
    waitProp "authentication" propState (== Authenticated)
    them <- getPeer
    debug "Adding peer"
    call addPeer them :: Client ()
    debug "Waiting for peer to be added"
    waitProp "peers" propPeers ((==1) . length)
    return ()


teardown :: Client ()
teardown = do
    setProp accountEnabledP False
    waitProp "disabled" propState (== Disabled)



main :: IO ()
main = runClient $ do
    setup
    teardown
    debug "Shutting down"
    return ()
