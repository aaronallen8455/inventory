{-# LANGUAGE RankNTypes #-}
module HieSource.T5 where

t5A :: Monad m => a -> m a
t5A = undefined

t5B :: forall b m. Monad m => b -> m b
t5B = undefined
