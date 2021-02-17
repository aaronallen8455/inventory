{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module HieSource.T17
  ( foo
  , D(..)
  , T(..)
  , C
  , TF
  , TF'
  , pattern P
  , TS
  ) where

import           Data.Void

data D = D { a :: Void, b :: Int }

deriving instance Show D

newtype T = T { unT :: Int }
  deriving (Eq, Show)

class C a where

instance C Int

type family TF a

type instance TF Bool = Int

type family TF' a where
  TF' Bool = Int

foo :: a -> a
foo = id

pattern P :: Int
pattern P = 4

type TS = ()
