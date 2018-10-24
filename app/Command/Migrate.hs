{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Command.Migrate where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.Configurator
import           Database.Persist.Postgresql
import           Options.Applicative

import           Comix.Data

data Options = Options
  { configFile :: FilePath
  , migrateDB  :: Bool
  }

process :: Options -> IO ()
process Options {..} = do
  cfg <- load [Required configFile]
  connStr <- require cfg "connStr"
  let migrate' = if migrateDB then runMigration else printMigration
  runStderrLoggingT
    $ withPostgresqlPool connStr 10
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
