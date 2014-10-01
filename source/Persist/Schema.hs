{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Persist.Schema where

import Control.Lens
import DBus (makeRepresentable)
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.TH
import Network.Xmpp (Jid)

import Persist.Stage
import Types

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "schema")

makeLensesWith (pLensRules "hostCredentials") ''HostCredentials

deriving instance Show HostCredentials

makeRepresentable ''Contact
