{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Time.Calendar
import Test.Hspec (hspec, it, describe)
import Test.Hspec.Expectations.Pretty

import Data.Csv
import Comix.Data

main :: IO ()
main = hspec $
  describe "tests for parsing" $
    it "should parse a simple example" $
      let txt = "Mike Mignola,Mike Mignola,,128,Hellboy: Seed of Destruction,2004-03-30,Dark Horse Books,*****"
          res = Comic { comicAuthor = "Mike Mignola"
                      , comicArtist = Just "Mike Mignola"
                      , comicLetterer = Nothing
                      , comicPages    = Just 128
                      , comicTitle    = "Hellboy: Seed of Destruction"
                      , comicPublished = fromGregorian 2004 03 30
                      , comicPublisher = "Dark Horse Books"
                      , comicRating = VeryGood
                      }
      in undefined
