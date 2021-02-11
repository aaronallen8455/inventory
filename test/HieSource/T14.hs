{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module HieSource.T14 where

import           Data.Proxy

t14A :: Proxy (a :: [b]) -> b -> k a b
t14A = undefined

t14B :: b -> Proxy (a :: [b]) -> k a b
t14B = undefined

t14C :: Proxy (a :: b) -> b -> k a b
t14C = undefined
