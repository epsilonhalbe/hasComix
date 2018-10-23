{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad
import           Options.Applicative

import           Command
import           Command.Capitalize  ()
import           Command.Migrate     ()
import           Command.Smallify    ()

main :: IO ()
main = join $ execParser (info cmd idm)
 where
  cmd = hsubparser $ mconcat
    [  subcmd @'Smallify
    ,  subcmd @'Capitalize
    ,  subcmd @'Migrate
    ]

