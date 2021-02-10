{-# LANGUAGE RankNTypes #-}
module HieSource.T6 where

t6A :: (Show a, Num b) => a -> b -> (a, b)
t6A = undefined

t6B :: (Num b, Show a) => a -> b -> (a, b)
t6B = undefined

t6C :: Num b => b -> Show a => a -> (a, b)
t6C = undefined
