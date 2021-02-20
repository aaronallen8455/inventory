{-# LANGUAGE MultiWayIf #-}
module MatchSigs.Matching.Env
  ( Env
  , (/\)
  , checkOr
  , checkAnd
  , introVars
  , tryAssignVar
  , initEnv
  ) where

import           Control.Monad.State.Strict
import           Data.List
import qualified Data.IntMap.Strict as IM

import           MatchSigs.Sig (FreeVarIdx)

type Level = Int
type VarLevel = IM.IntMap Int
type VarAssign = IM.IntMap FreeVarIdx

-- | Context for matching the free vars in two 'Sigs'
data Env =
  MkEnv { level    :: !Level -- current var level
        , vass     :: !VarAssign -- map from B vars to A vars
        , vlA      :: !VarLevel -- the level at which an A var with introduced
        , vlB      :: !VarLevel
        }

initEnv :: Env
initEnv =
  MkEnv { level    = 0
        , vass     = mempty
        , vlA      = mempty
        , vlB      = mempty
        }

-- | Identify var from one sig with var in other sig
tryAssignVar :: FreeVarIdx
             -> FreeVarIdx
             -> State Env Bool
tryAssignVar ai bi = do
  env <- get
  let mb = IM.lookup bi $ vass env
  if -- already assigned
     | Just x <- mb
     , x == ai -> pure True

     -- not assigned and levels match
     | Nothing <- mb
     , Just lA <- IM.lookup ai $ vlA env
     , Just lB <- IM.lookup bi $ vlB env
     , lA == lB
     -> do put env { vass = IM.insert bi ai $ vass env }
           pure True

     | otherwise -> pure False

-- | Add vars from both sigs to the context, accounting for level
introVars :: [FreeVarIdx]
          -> [FreeVarIdx]
          -> State Env Bool
introVars [] [] = pure True
introVars va vb
  | length va == length vb
  = (True <$) . modify' $ \env ->
      let lvl = level env
       in env { vlA = IM.fromList (zip va $ repeat lvl) <> vlA env
              , vlB = IM.fromList (zip vb $ repeat lvl) <> vlB env
              , level = lvl + 1
              }
  | otherwise = pure False

-- | Logical conjuction
(/\) :: State env Bool
     -> State env Bool
     -> State env Bool
a /\ b = do
  r <- a
  if r then b else pure False

checkAnd :: [State Env Bool]
         -> State Env Bool
checkAnd = foldl' (/\) (pure True)

-- | Logical disjunction. Discards state if False
(\/) :: State env Bool
     -> State env Bool
     -> State env Bool
a \/ b = StateT $ \env ->
  let (ar, as) = runState a env
      ~(br, bs) = runState b env
   in if ar then pure (ar, as)
            else if br then pure (br, bs)
                 else pure (False, env)

checkOr :: [State env Bool]
        -> State env Bool
checkOr = foldl' (\/) (pure False)

