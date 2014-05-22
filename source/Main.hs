{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Crypto.PubKey.DSA
import Data.Int
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID
import Database.Persist.Quasi
import Database.Persist.TH
import Network.Xmpp (Jid)

import Persist

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "schema")

main = undefined
