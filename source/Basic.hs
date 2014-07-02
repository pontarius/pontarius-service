{-# LANGUAGE OverloadedStrings #-}
module Basic where

import           Control.Lens
import           DBus
import           Data.Text (Text)
import qualified Data.Text as Text
import           Types

pontariusObjectPath :: ObjectPath
pontariusObjectPath = objectPath "pontarius"

pontariusInterface :: Text
pontariusInterface = "org.pontarius"
