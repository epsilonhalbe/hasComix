{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Command where

import           Data.List.Extra
import           Options.Applicative

data Command
  = Smallify
  | Capitalize
  | Migrate

-- | Singleton GADT to demote type level information to value level
data SCommand (a :: Command) where
  SSmallify   :: SCommand 'Smallify
  SCapitalize :: SCommand 'Capitalize
  SMigrate    :: SCommand 'Migrate

deriving instance Show (SCommand a)

type SubCmd (a :: Command) = Mod CommandFields (IO ())

class Process (a :: Command) where
  data Options a :: *
  sing :: SCommand a
  process :: Options a -> IO ()
  options :: Parser (Options a)
  {-# MINIMAL process, options, sing #-}
  subcmd :: SubCmd a
  subcmd = let commandname = lower . tail $ show @(SCommand a) sing
               process' = process :: Options a -> IO ()
               options' = options <**> helper
            in command commandname (info (process' <$> options') idm)



