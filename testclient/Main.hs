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
import           System.FilePath
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger

import           PontariusService.Types

import           Interface
import           Test

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
    r <- DBus.call initialize peer () [] con :: IO (Either MethodError PontariusState)
    case r of
     Left e -> do
         liftIO $ threadDelay 100000 -- 100ms
         waitForServer con
     Right _ -> return ()

logDir = "/logs/client"

makeLogger component loggerNames filename = do
    hndlr <- fileHandler (logDir  </> filename) DEBUG
    forM_ loggerNames $ \loggerName ->
        updateGlobalLogger loggerName $ setLevel DEBUG . setHandlers [hndlr]


runClient :: Client b -> IO b
runClient f = do
    ln
    yellow "Running Tests" >> ln
    putStrLn "-------------------"
    args <- liftIO getArgs
    let (credentials, them) =
            case component of
             Active -> (credentialsA, fst credentialsB)
             Passive -> (credentialsB, fst credentialsA)
    updateGlobalLogger rootLoggerName $ removeHandler . removeHandler
    let mkLogger = makeLogger component
    mkLogger ["DBus.Reactive", "DBus"] "dbus"
    mkLogger ["Pontarius.Xmpp"] "pontarius-xmpp"
    con <- try "Connecting to DBus session" $
              connectBus Session ignore ignore
    try "Waiting for server" $ waitForServer con
    statusVar <- propertyToTVar statusP con
    peersVar <- propertyToTVar peersP con
    enabledVar <- propertyToTVar accountEnabledP con
    let st = ClientState
               { clientGlobalsConnection = con
               , clientGlobalsPropState = statusVar
               , clientGlobalsPropAccountEnabled = enabledVar
               , clientGlobalsPropPeers = peersVar
               , clientGlobalsCredentials = credentials
               , clientGlobalsThem = them
               }
    runReaderT (unClient $ dumpStateOnException f) st

setup :: Client ()
setup = do
    creds <- getXMPPCredentials
    () <- call setCredentials creds

    idents <- try "Setting credentials" $
                call getIdentities () :: Client [Text]
    try "Creating identity" $
        case idents of
         [] -> do
             _ <- call createIdentity () :: Client ByteString
             return ()
         (ident:_) -> call setIdentity ident
    try "Enabling account" $ setProp accountEnabledP True
    try "Waiting for XMPP connection to complete" $
      waitProp "authentication" propState (== Authenticated)
    them <- getPeer
    try "Adding peer" $ call addPeer them :: Client ()
    try "Waiting for peer to be added"
      $ waitProp "peers" propPeers ((==1) . length)
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
