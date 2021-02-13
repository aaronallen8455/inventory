module HieSource.T16 where

newtype T = T { f :: () }

instance Show T where
  show _ = ""

foo :: T -> T
foo a = a
