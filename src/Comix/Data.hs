{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Comix.Data where

import           Data.Csv            (FromRecord, ToRecord)
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Data.Time.Orphans   ()
import           Database.Persist.TH
import           GHC.Generics        (Generic)

import           Comix.Data.Rating

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Comic
  <fill in stuff>
  deriving Show Eq
|]

deriving instance Generic Comic
instance FromRecord Comic
instance ToRecord Comic

