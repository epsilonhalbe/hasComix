{-# LANGUAGE ScopedTypeVariables #-}
module Comix.Lib where

import           Data.Text (Text)
import qualified Data.Text as T

capitalize :: Text -> Text
capitalize text =
  let inputLines :: [(Int, Text)] = zip [1 ..] $ T.lines text
      filteredLines :: [Text]     = map snd $ filter (odd . fst) inputLines
  in  T.toUpper $ T.unlines filteredLines

smallify :: Text -> Text
smallify text =
  let inputLines :: [(Int, Text)] = zip [1 ..] $ T.lines text
      filteredLines :: [Text]     = map snd $ filter (even . fst) inputLines
  in  T.toLower $ T.unlines filteredLines
