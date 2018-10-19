module Command where

import           Options.Applicative

data Command
  = Smallify
  | Capitalize
  | Migrate
  deriving Show

subcmd :: String -> (a -> IO ()) -> Parser a -> Mod CommandFields (IO ())
subcmd name process options =
  let options' = options <**> helper
   in command name (info (process <$> options') idm)


