{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Comix.Data
  ( module Comix.Data
  , Rating(..)
  ) where

import           Data.Csv            (FromRecord, ToRecord)
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Data.Time.Orphans   ()
import           Database.Persist.TH
import           GHC.Generics        (Generic)

import           Comix.Data.Rating

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Comic
  author    Text
  artist    Text Maybe
  letterer  Text Maybe
  pages     Word Maybe
  title     Text
  published Day
  publisher Text
  rating    Rating
  deriving Show Eq
  UniqueAuthorTitle author title
|]

deriving instance Generic Comic
instance FromRecord Comic
instance ToRecord Comic

