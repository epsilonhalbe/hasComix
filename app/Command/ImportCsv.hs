{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Command.ImportCsv where

import           Control.Monad.IO.Class
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.Char                    (ord)
import           Data.Conduit                 as C
import           Data.Conduit.Binary          as C
import           Data.Conduit.List            as CL
import           Data.Conduit.Combinators     as C
import           Data.Csv                     (DecodeOptions(..), HasHeader(NoHeader), decodeWith)
import qualified Data.Vector                  as V
import           Options.Applicative
import           System.Directory
import           System.FilePath

import           Comix.Data

data Options = Options
  { outFolder    :: FilePath
  , inFolder     :: FilePath
  , csvSeparator :: DecodeOptions
  }

process :: Options -> IO ()
process Options {..} = do
  putStrLn "Begin Processing"
  x <- runConduitRes
       $ C.sourceDirectory inFolder
      .| C.filterM (liftIO . doesFileExist)
      .| C.filter (isExtensionOf "csv")
      .| C.awaitForever C.sourceFile
      .| C.lines
      .| decodeWith' csvSeparator
      .| CL.consume
  Prelude.print (x :: [Either String Comic])
  putStrLn "End Processing"

decodeWith' :: MonadIO m => DecodeOptions -> ConduitT ByteString (Either String Comic) m ()
decodeWith' decOpts = do
  x <- await
  case fromStrict <$> x of
    Nothing -> return ()
    Just bs -> yield $
      case decodeWith decOpts NoHeader bs of
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


