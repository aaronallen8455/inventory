module HieSource.T16 where

newtype T = T { f :: () }

instance Show T where
  show x = show $ foo x

foo :: T -> T
foo a@T{ f = () } = a
