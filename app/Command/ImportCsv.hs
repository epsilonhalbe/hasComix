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
       $ _read -- in a whole directory don't use normal functions, but
               -- read up on Data.Conduit.Combinators and use C.functionname
      .| _1 (liftIO . _2) -- get rid of the non directory stuff - files only
                                        -- note: this is a side effecty function
      .| C.filter _only -- keep the files with 'csv' extension
                        -- note: this time we use pure code
      .| C.awaitForever _this -- is rather tricky: read in the file
                              -- contents, but the standard conduit
                              -- functions for reading in file data needs a
                              -- filepath - C.awaitForever should solve this
                              -- problem
      .| _split -- into each line
      .| decodeWith' csvSeparator
      .| CL.consume
  Prelude.print x
  putStrLn "End Processing"

-- the next step is an exercise in manually crafting streaming code
-- • `await` is like "next()" in an iterator based approach and gives you
--   the next element in the stream - or Nothing which makes it a
--   `Maybe ByteString` in this case.
-- • to stop a stream you can always `return ()`
-- • `yield` allows you to push a value back into the stream
decodeWith' :: MonadIO m => DecodeOptions -> ConduitT ByteString (Either String Comic) m ()
decodeWith' decOpts = do
  _next -- replace me
  case fromStrict <$> next of
    Nothing -> _nothing
    Just bs -> yield $ case _process_the_bytestring bs of
                -- take a look at Data.Csv it should contain all the
                -- information to decode the bytestring, but be aware
                -- that we don't have a header for the CSV as we
                -- process each line, and we can pass in the csv delimiter as an
                -- option
                 Right res -> _process_the_result
                 -- we need to check if this `V.Vector Comic` has only
                 -- one element, unfortunately Data.Csv doesn't
                 -- provide a function to decode a single line.
                 Left l -> Left l
-- Note: we have `Either String (Vector Comic) -> Either String Comic`
--       if we did write `l -> l` the type signature would be
--       `Either String (Vector Comic) -> Either String (Vector Comic)`

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
    <*> _decode_options
      -- this should be a DecodeOptions, with default value ','
      -- Char does not work like strOptions - but you can use `option auto`


