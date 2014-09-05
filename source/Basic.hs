{-# LANGUAGE OverloadedStrings #-}
module Basic where

import Control.Monad.Trans
import DBus
import Data.Text (Text)
import System.IO


pontariusObjectPath :: ObjectPath
pontariusObjectPath = "/pontarius"

pontariusInterface :: Text
pontariusInterface = "org.pontarius"

debug :: MonadIO m => String -> m ()
debug = liftIO . hPutStrLn stderr
