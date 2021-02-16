module HieSource.T8 where

t8A :: a -> Int -> (a, Int)
t8A = undefined

t8B :: a -> Int -> (Int, a)
t8B = undefined

t8C :: a -> Int -> (Int, (Int -> a, a))
t8C = undefined

t8D :: Int -> a -> ((a, Int -> a), Int)
t8D = undefined
