{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Command.Migrate where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.String
import           Database.Persist.Postgresql
import           Dhall
import           Options.Applicative

import           Comix.Data

newtype Config = Config
  { connStr :: String
  } deriving (Generic, Show)

instance Interpret Config

data Options = Options
  { configFile :: FilePath
  , migrateDB :: Bool
  }

process :: Options -> IO ()
process Options {..} = do
  Config{..} <- input Dhall.auto $ fromString configFile
  let migrate' = if migrateDB then runMigration else printMigration
  runStderrLoggingT
    $ withPostgresqlPool (fromString connStr) 10
    $ liftIO
    . runSqlPersistMPool (migrate' migrateAll)

options :: Parser Options
options =
  Options
    <$> strOption
          (  long "config-file"
          <> short 'c'
          <> metavar "PATH"
          <> help "Specifies the config file."
          <> showDefault
          <> value "./conf/database.conf"
          )
    <*> switch
          (  long "run-migration"
          <> help "If present, runs the migration otherwise only the SQL-migration is printed to stdout"
          )
