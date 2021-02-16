{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module HieSource.T20 where

import           Data.Proxy

t20A :: Proxy (a :: [b] -> Bool -> Either b c) -> b -> c
t20A = undefined

t20B :: b -> Proxy (a :: Bool -> [b] -> Either b c) -> c
t20B = undefined

t20C :: b -> Proxy (a :: Bool -> [b] -> Either c b) -> c
t20C = undefined

t20D :: b -> Proxy (a :: Bool -> Either c b -> [b]) -> c
t20D = undefined
