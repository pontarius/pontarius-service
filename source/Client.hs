{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Client where

import           Control.Monad
import           Control.Monad.IO.Class
import           DBus
import           DBus.Introspect
import           DBus.Scaffold
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH.Syntax

$(do
    let interfaceFile = "dbus-interface.xml"
    qAddDependentFile interfaceFile
    xml <- qRunIO $ BS.readFile interfaceFile
    let Right node = xmlToNode xml
        methods = nodeMethodDescriptions "" node
    fmap concat . forM methods
        $ methodFunction (Text.unpack . methodMember) Nothing
 )
