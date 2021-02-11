{-# LANGUAGE RankNTypes #-}
module HieSource.T11 where

t11A :: a -> (b -> Int) -> a
t11A = undefined

t11B :: a -> (forall b. b -> Int) -> a
t11B a _ = a
