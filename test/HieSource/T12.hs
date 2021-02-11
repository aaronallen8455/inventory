module HieSource.T12 where

t12A :: Monad m => a -> Either (m a) Int
t12A = undefined

t12B :: Functor m => a -> Either (m a) Int
t12B = undefined
