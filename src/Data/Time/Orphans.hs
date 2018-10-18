module Data.Time.Orphans where

import Data.Time
import Data.Csv
import Data.ByteString.Char8(unpack)

instance ToField Day where
  toField = toField . show

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack
