{-# LANGUAGE DeriveDataTypeable #-}
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
-- Helper module for test handling
module Test where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import qualified Control.Monad.Catch as Ex
import           Control.Monad.Reader
import           Control.Monad.Trans
import           DBus
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Console.ANSI
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Time.Clock
import           System.Timeout

import           PontariusService.Types

data Timeout = Timeout deriving (Show, Eq, Data, Typeable)

instance Ex.Exception Timeout

withColor :: ColorIntensity -> Color -> IO () -> IO ()
withColor mod color f = do
    s <- hSupportsANSI stdout
    when s $ setSGR [SetColor Foreground mod color]
    f
    when s $ setSGR [Reset]

cPutStr :: ColorIntensity -> Color -> String -> IO ()
cPutStr mod color str = withColor mod color (putStr str)

green :: MonadIO m => String -> m ()
green = liftIO . cPutStr Dull Green

red :: MonadIO m => String -> m ()
red = liftIO . cPutStr Vivid Red

yellow :: MonadIO m => String -> m ()
yellow = liftIO . cPutStr Dull Yellow

ln = do
    liftIO $ putStrLn ""
    liftIO $ hFlush stdout

ok :: MonadIO m => m ()
ok = do
    green "OK."
    ln

failed :: MonadIO m => m ()
failed = do
    red "Failed."
    ln

data Component = Active | Passive deriving (Show, Eq)

component :: Component
component = unsafePerformIO $ do
    args <- liftIO getArgs
    case args of
        ["active"] -> return Active
        ["passive"] -> return Passive
        _ -> liftIO $ do
            hPutStrLn stderr "Usage: testclient [active|passive]"
            exitFailure
{- NoInline component -}

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

debug :: String -> Client ()
debug str = case component of
             Active -> yellow str >> ln
             Passive -> return ()

dumpState :: Client ()
dumpState = do
    ln
    yellow "Dumping State" >> ln
    globals <- Client ask
    mapM_ (\(name, getter) -> do
               SSTV tv <- return $ globals ^. getter
               v <- liftIO . atomically $ readTVar tv
               liftIO $ do
                 putStr " - "
                 yellow name
                 putStrLn $ ": " ++ show v)
        [ ("pontarius state", propState . shw)
        , ("account enabled", propAccountEnabled .shw)
        , ("peers", propPeers . shw)
        ]
  where shw = to SSTV

dumpStateOnException :: Client a -> Client a
dumpStateOnException f =
    Ex.catch f $ \(e :: Ex.SomeException) -> dumpState >> liftIO exitFailure

withTimeout :: String -> Client a -> Client a
withTimeout message (Client f) = do
    st <- Client ask
    res <- liftIO $ timeout (10*10^6) (runReaderT f st)
    case res of
     Nothing -> Ex.throwM $ Timeout
     Just r -> return r

try :: (MonadIO m, Ex.MonadCatch m) => String -> m a -> m a
try descr f = do
    liftIO . putStr $ descr ++ ": "
    liftIO $ hFlush stdout
    start <- liftIO getCurrentTime
    Ex.catch (do res <- f
                 end <- liftIO getCurrentTime
                 green $ "OK. (" ++ show (diffUTCTime end start) ++ ")"
                 ln
                 return res
             )
        $ \(e :: Ex.SomeException) -> do
          red $ "Exception: " ++ show e
          ln
          Ex.throwM e
