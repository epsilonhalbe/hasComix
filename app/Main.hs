module Main where

import Control.Monad
import Options.Applicative

import           Command
import qualified Command.Capitalize  as Capitalize
import qualified Command.Migrate     as Migrate
import qualified Command.Smallify    as Smallify

main :: IO ()
main = join $ execParser (info cmd idm)
 where
  cmd = hsubparser $ mconcat
    [ Command.subcmd "capitalize" Capitalize.process Capitalize.options
    , Command.subcmd "migrate"    Migrate.process    Migrate.options
    , Command.subcmd "smallify"   Smallify.process   Smallify.options
    ]
