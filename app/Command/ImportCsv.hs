{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Command.ImportCsv where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8        (ByteString, pack)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.Char                    (ord)
import           Data.Conduit                 as C
import           Data.Conduit.Binary          as C
import           Data.Conduit.Combinators     as C
import           Data.Configurator
import           Data.Csv                     (DecodeOptions (..),
                                               HasHeader (NoHeader), decodeWith)
import qualified Data.Text.Encoding           as T
import qualified Data.Vector                  as V
import           Database.Persist.Postgresql
import           Options.Applicative
import           System.Directory
import           System.FilePath

import           Comix.Data

data Options = Options
  { outFolder    :: FilePath
  , inFolder     :: FilePath
  , csvSeparator :: DecodeOptions
  , configFile   :: FilePath
  }

process :: Options -> IO ()
process Options {..} = do
  putStrLn "Begin Processing"
  cfg  <- load [Required configFile]
  connStr <- require cfg "connStr"
  pool :: ConnectionPool <- runNoLoggingT $ createPostgresqlPool connStr 4
  runConduitRes
       $ C.sourceDirectory inFolder
      .| C.filterM (liftIO . doesFileExist)
      .| C.filter (isExtensionOf "csv")
      .| C.awaitForever C.sourceFile
      .| C.lines
      .| C.awaitForever (decodeWith' csvSeparator)
      .| C.awaitForever (insertWith' pool)
      .| C.stdout
  putStrLn "End Processing"

insertWith' :: ConnectionPool -> Either String Comic -> ConduitT (Either String Comic) ByteString ResIO ()
insertWith' _    (Left l     ) = yield ("\tNOK: " <> pack l)
  where pretty = let (beg,mid) = split
insertWith' pool (Right comic) = do
  ok <- liftIO $ runSqlPool (upsert comic []) pool
  yield $ mconcat
          [ "\t OK: inserted or updated ["
          , pack $ show $ unSqlBackendKey $ unComicKey $ entityKey ok
          , "] "
          , T.encodeUtf8 $ comicTitle $ entityVal ok
          , "\n"
          ]


decodeWith' :: MonadIO m => DecodeOptions -> ByteString -> ConduitT ByteString (Either String Comic) m ()
decodeWith' decOpts bs = yield $
      case decodeWith decOpts NoHeader $ fromStrict bs of
        Right res ->
          if V.null res || V.length res > 1
            then Left $ "Unexpected result:" <> show res
            else Right $ V.head res
        Left l -> Left l



options :: Parser Options
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
    <*> (DecodeOptions . fromIntegral . ord
        <$> option
          auto
          (  long "separator"
          <> short 's'
          <> metavar "CHAR"
          <> help "Specifies the CSV separator character."
          <> showDefault
          <> value ','
          ))
    <*> strOption
          (  long "config-file"
          <> short 'c'
          <> metavar "PATH"
          <> help "Specifies the config file."
          <> showDefault
          <> value "./conf/database.conf"
          )


