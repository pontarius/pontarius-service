{-# LANGUAGE OverloadedStrings #-}
module Basic where

import           DBus
import           Data.Text (Text)

pontariusObjectPath :: ObjectPath
pontariusObjectPath = "/pontarius"

pontariusInterface :: Text
pontariusInterface = "org.pontarius"
