module HieSource.T18 where

import qualified Data.IntMap.Strict as IM

data Sig a = X a | Y a

type FreeVarIdx = Int

t18A :: IM.IntMap FreeVarIdx
     -> [Sig FreeVarIdx]
     -> [Sig FreeVarIdx]
     -> Bool
t18A _ [] = undefined
t18A _ _ = undefined

t18B :: IM.IntMap FreeVarIdx
     -> [Sig FreeVarIdx]
     -> [Sig FreeVarIdx]
     -> Bool
t18B = undefined

t18C :: IM.IntMap FreeVarIdx
     -> [Sig FreeVarIdx]
     -> [Sig FreeVarIdx]
     -> Bool
t18C = undefined

t18D :: IM.IntMap FreeVarIdx
     -> [Sig FreeVarIdx]
     -> [Sig FreeVarIdx]
     -> Bool
t18D = undefined
