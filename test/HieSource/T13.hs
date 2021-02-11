{-# LANGUAGE RankNTypes #-}
module HieSource.T13 where

t13A :: Monad m => (forall b m. Monad m => a -> (m (a -> b), m b)) -> m Int
t13A _ = undefined

t13B :: Monad m => (forall b m. Monad m => a -> (m b, m (a -> b))) -> m Int
t13B _ = undefined

