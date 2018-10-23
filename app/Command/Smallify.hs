{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Command.Smallify where

import           Control.Monad
import           Control.Monad.Extra
import           Data.Semigroup      ((<>))
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.Directory
import           System.FilePath

import           Comix.Lib
import           Command

instance Process 'Smallify where

  data Options 'Smallify = Options
    { outFolder       :: FilePath
    , inFolder        :: FilePath
    , outExtension    :: String
    , appendExtension :: Bool
    }
  process Options {..} = do
    filePaths <- filterM doesFileExist =<< listDirectory inFolder
    unlessM (doesDirectoryExist outFolder) (createDirectory outFolder)
    let to f x = outFolder </> (if appendExtension then (<.>) else (-<.>)) f x
    forM_ filePaths $ \file ->
      T.writeFile (to file outExtension) . smallify =<< T.readFile file
  sing = SSmallify
  options =
    Options
      <$> strOption
            (  long "output-folder"
            <> short 'o'
            <> metavar "DIR"
            <> help "Specifies the output folder."
            <> showDefault
            <> value "outFiles"
            )
      <*> strOption
            (  long "input-folder"
            <> short 'i'
            <> metavar "DIR"
            <> help "Specifies the input folder."
            <> showDefault
            <> value "."
            )
      <*> option
            str
            (  long "extension"
            <> short 'e'
            <> metavar "STRING"
            <> help "Specifies the extension."
            <> showDefault
            <> value "out"
            )
      <*> switch
            (long "append-extension" <> short 'a' <> help
              "If present, the extension is appended instead of replaced."
            )



