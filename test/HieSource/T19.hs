{-# LANGUAGE RankNTypes #-}
module HieSource.T19 where

t19A :: forall a. a -> forall b. b -> Either a b
t19A _ _ = undefined

t19B :: forall b a. a -> b -> Either a b
t19B = undefined

t19C :: forall b a. b -> a -> Either a b
t19C = undefined
