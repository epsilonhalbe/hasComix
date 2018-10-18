{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Orphans
import Data.Csv
import GHC.Generics (Generic)
import Control.Monad

data Comic = Comic {
    author :: Text
  , artist :: Maybe Text
  , letterer :: Maybe Text
  , pages :: Maybe Word
  , title :: Text
  , published :: Day
  , publisher :: Text
  , rating :: Rating
} deriving (Show, Eq, Generic)

instance FromRecord Comic
instance ToRecord Comic

data Rating = VeryBad | Bad | OK | Good | VeryGood
  deriving (Eq, Bounded, Enum)

instance Show Rating where
  show = (`replicate` '*') . succ . fromEnum

instance FromField Rating where
  parseField s
    | s == "*" = pure VeryBad
    | s == "**" = pure Bad
    | s == "***" = pure OK
    | s == "****" = pure Good
    | s == "*****" = pure VeryGood
    | otherwise = mzero

instance ToField Rating where
  toField = toField . show

capitalize :: Text -> Text
capitalize text =
  let inputLines :: [(Int, Text)] = zip [1..] $ T.lines text
      filteredLines :: [Text] = map snd $ filter (odd . fst) inputLines
  in T.toUpper $ T.unlines filteredLines

smallify :: Text -> Text
smallify text =
  let inputLines :: [(Int, Text)] = zip [1..] $ T.lines text
      filteredLines :: [Text] = map snd $ filter (even . fst) inputLines
  in T.toLower $ T.unlines filteredLines
