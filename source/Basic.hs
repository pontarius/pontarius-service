{-# LANGUAGE OverloadedStrings #-}
module Basic where

import DBus
import Data.Text (Text)
import System.IO

pontariusObjectPath :: ObjectPath
pontariusObjectPath = "/pontarius"

pontariusInterface :: Text
pontariusInterface = "org.pontarius"

debug :: String -> IO ()
debug = hPutStrLn stderr
