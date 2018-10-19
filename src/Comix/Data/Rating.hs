{-# LANGUAGE OverloadedStrings #-}
module Comix.Data.Rating where

{- separate from Data because in template haskell(TH) code that TH depends on
   needs to be compiled before the TH code itself, for normal haskell code
   dependent code can be compiled at the same time, reason is that haskell
   (unlike GO) compiles multi staged - but since TH is compile time meta
   programming - i.e. one can use haskell code to generate haskell code which
   then is compiled, thus TH is actually type safe.

   N.B. The usage of TH is a bit of a discussion point in the community, because
   it prevents you from doing things like cross compilation. Nowadays "Generic"
   is a powerful enough mechanism to replace TH in a lot of cases.
-}
import           Control.Monad        (mzero)
import           Data.Bifunctor       (second)
import           Data.Csv             (FromField (..), ToField (..))
import           Data.Proxy           (Proxy (..))
import           Database.Persist.Sql (PersistField (..), PersistFieldSql (..))

data Rating = VeryBad | Bad | OK | Good | VeryGood
  deriving (Eq, Bounded, Enum)

instance PersistField Rating where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = second toEnum . fromPersistValue

instance PersistFieldSql Rating where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance Show Rating where
  show = (`replicate` '*') . succ . fromEnum

instance FromField Rating where
  parseField s
    | s == "*" = pure VeryBad
    | s == "**" = pure Bad
    | s == "***" = pure OK
    | s == "****" = pure Good
    | s == "*****" = pure VeryGood
    | otherwise = mzero

instance ToField Rating where
  toField = toField . show

