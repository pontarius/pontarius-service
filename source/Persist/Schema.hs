{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Persist.Schema where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Xmpp (Jid)

import           Persist.Stage
import           Types

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "schema")
