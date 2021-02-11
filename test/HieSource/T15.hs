module HieSource.T15 where

class C a where
  c :: a -> ()

instance C Int where
  c _ = ()
