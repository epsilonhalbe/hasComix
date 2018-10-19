module Main where

import Lib
import qualified Data.Text.IO as T
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Extra
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text(Text)

import           Command
import qualified Command.Capitalize  as Capitalize
import qualified Command.Migrate     as Migrate
import qualified Command.Smallify    as Smallify

data Command = Smallify Options
  | Capitalize Options
  deriving (Show, Eq)


main :: IO ()
main = join $ execParser (info cmd idm)
 where
  cmd = hsubparser $ mconcat
    [ Command.subcmd "capitalize" Capitalize.process Capitalize.options
    , Command.subcmd "migrate" Migrate.process Migrate.options
    , Command.subcmd "smallify" Smallify.process Smallify.options
    ]


  where
    cmd = hsubparser
      (command "smallify" (info (smallifyIO <$> (options <**> helper)) idm)
      <> command "capitalize" (info (capitalizeIO <$> (options <**> helper)) idm))

processFiles :: (Text -> Text) -> Options -> IO ()
processFiles run  Options{..} = do
  filePaths <- filterM doesFileExist =<< listDirectory inFolder
  unlessM (doesDirectoryExist outFolder) (createDirectory outFolder)
  let to f x = outFolder </> (if appendExtension then (<.>) else (-<.>)) f x
  forM_ filePaths $ \file -> do
    T.writeFile (to file outExtension) . run =<< T.readFile file

smallifyIO, capitalizeIO :: Options -> IO ()
smallifyIO = processFiles smallify
capitalizeIO = processFiles capitalize


options :: Parser Options
options = Options
  <$> strOption
    (long "output-folder"
    <> short 'o'
    <> metavar "DIR"
    <> help "Specifies the output folder."
    <> showDefault
    <> value "outFiles")
  <*> strOption
    (long "input-folder"
    <> short 'i'
    <> metavar "DIR"
    <> help "Specifies the input folder."
    <> showDefault
    <> value ".")
  <*> option str
    (long "extension"
    <> short 'e'
    <> metavar "STRING"
    <> help "Specifies the extension."
    <> showDefault
    <> value "out")
  <*> switch
    (long "append-extension"
    <> short 'a'
    <> help "If present, the extension is appended instead of replaced.")


-- next: Applicative (optparse-Applicative)
