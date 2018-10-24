{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Time.Calendar
import Test.Hspec (hspec, it, describe)
import Test.Hspec.Expectations.Pretty
import Data.ByteString.Lazy
import Data.Text.Encoding

import Data.Csv
import Comix.Data

main :: IO ()
main = hspec $
  describe "tests for parsing" $ do
    it "should parse a simple example" $
      let txt = fromStrict $ encodeUtf8  "Mike Mignola,Mike Mignola,,128,Hellboy: Seed of Destruction,2004-03-30,Dark Horse Books,*****"
          res = Comic { comicAuthor = "Mike Mignola"
                      , comicArtist = Just "Mike Mignola"
                      , comicLetterer = Nothing
                      , comicPages    = Just 128
                      , comicTitle    = "Hellboy: Seed of Destruction"
                      , comicPublished = fromGregorian 2004 03 30
                      , comicPublisher = "Dark Horse Books"
                      , comicRating = VeryGood
                      }
      in decode NoHeader txt `shouldBe` Right [res]
    it "should parse a complex example" $
      let txt = fromStrict $ encodeUtf8 "Kōhei Horikoshi,,,0,\"僕のヒーローアカデミア, Boku no Hīrō Akademia\",2014-07-07,Shueisha,****"
          res = Comic { comicAuthor = "Kōhei Horikoshi"
                      , comicArtist = Nothing
                      , comicLetterer = Nothing
                      , comicPages    = Just 0
                      , comicTitle    = "僕のヒーローアカデミア, Boku no Hīrō Akademia"
                      , comicPublished = fromGregorian 2014 07 07
                      , comicPublisher = "Shueisha"
                      , comicRating = Good
                      }
      in decode NoHeader txt `shouldBe` Right [res]


