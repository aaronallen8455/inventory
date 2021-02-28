module MatchSigs.Matching
  ( MatchedSigs(..)
  ) where

import           Control.Monad.State.Strict
import           Data.List

import           GHC.Api
import           MatchSigs.Matching.Env
import           MatchSigs.Sig

type SigMatches = ( [Sig FreeVarIdx] -- Sig shared by these 'Name's
                  , String -- rendered sig
                  , [Name] -- Names that share this signature
                  )

newtype MatchedSigs =
  MatchedSigs { getMatchedSigs :: [SigMatches] }

instance Semigroup MatchedSigs where
  (<>) = unionMatchedSigs

instance Monoid MatchedSigs where
  mempty = MatchedSigs mempty

-- | Create the union of two 'MatchedSigs' by checking if there a match in one
-- group for each sig in the other.
-- This is O(n^2) since there is no suitable ordering for sigs due to different
-- potential ordering of free vars.
unionMatchedSigs :: MatchedSigs -> MatchedSigs -> MatchedSigs
unionMatchedSigs (MatchedSigs a) (MatchedSigs b)
  = MatchedSigs
  . uncurry (++)
  -- fold compatible sigs from b in a, append the ones that are not compatible
  $ foldl' go (a, []) b
  where
    go (aSigs, nonMatches) bSig
      = let check (ss, False) aSig
              = case compatibleSigs aSig bSig of
                  Just s' -> (s' : ss, True)
                  Nothing -> (aSig : ss, False)
            check (ss, True) aSig = (aSig : ss, True)
         in case foldl' check ([], False) aSigs of
              (res, False) -> (res, bSig : nonMatches)
              (res, True) -> (res, nonMatches)

-- | Combines the names in two 'SigMatches' if the sigs match
compatibleSigs :: SigMatches -> SigMatches -> Maybe SigMatches
compatibleSigs (sigA, str, namesA) (sigB, _, namesB) =
  if evalState (checkMatch sigA sigB) initEnv
     then Just (sigA, str, namesA ++ namesB)
     else Nothing

-- | Check that two sigs are isomorphic
-- First step is to check that the contexts match.
checkMatch :: [Sig FreeVarIdx]
           -> [Sig FreeVarIdx]
           -> State Env Bool
-- VarCtx and Qual are both expected to occur at the front of the list
checkMatch (VarCtx va : restA) (VarCtx vb : restB)
  = introVars va vb
 /\ checkMatch restA restB
checkMatch (VarCtx _ : _) _ = pure False
checkMatch _ (VarCtx _ : _) = pure False

-- Appearance order of quals not significant
checkMatch (Qual qa : restA) bs@(Qual _ : _) =
  let (qualsB, restB) = span isQual bs
      splits = zip (inits qualsB) (tails qualsB)
      go (i, Qual f : rest)
        = checkMatch qa f
       /\ checkMatch restA (i ++ rest ++ restB)
      go _ = pure False
   in checkOr $ go <$> splits
checkMatch (Qual _ : _) _ = pure False
checkMatch _ (Qual _ : _) = pure False

checkMatch sa sb = checkResult sa sb

-- | Extract the result types and make sure they match before going any further.
checkResult :: [Sig FreeVarIdx]
            -> [Sig FreeVarIdx]
            -> State Env Bool
checkResult sa sb
  | ra : restA <- reverse sa
  , rb : restB <- reverse sb
  = checkArguments [ra] [rb]
 /\ checkArguments restA restB
checkResult _ _ = pure True

-- | After the result type has been removed, check the argument types.
checkArguments :: [Sig FreeVarIdx]
               -> [Sig FreeVarIdx]
               -> State Env Bool
checkArguments [] [] = pure True
checkArguments (FreeVar ai : restA) (FreeVar bi : restB)
  = tryAssignVar ai bi
 /\ checkArguments restA restB

checkArguments (TyDescriptor sa na : restA) (TyDescriptor sb nb : restB)
  | sa == sb
  , na == nb
  = checkArguments restA restB
  | otherwise = pure False

-- this is where we need to check for a failure and rotate the list
checkArguments (Arg aa : restA) sb =
  let splits = zip (inits sb) (tails sb)
      go (i, Arg ab : rest)
        = checkMatch aa ab
       /\ checkArguments restA (i ++ rest)
      go _  = pure False
   in checkOr $ go <$> splits

checkArguments (Apply ca aa : restA) (Apply cb ab : restB)
  | length aa == length ab
  = checkMatch ca cb
 /\ checkAnd (zipWith checkMatch aa ab)
 /\ checkArguments restA restB
  | otherwise = pure False

checkArguments (Tuple [] : restA) (Tuple [] : restB)
  = checkArguments restA restB
checkArguments (Tuple (a : as) : restA) (Tuple bs : restB)
  | length as + 1 == length bs
  , let splits = zip (inits bs) (tails bs)
        go (i, b : rest)
            = checkMatch a b
           /\ checkArguments [Tuple as] [Tuple $ i ++ rest]
           /\ checkArguments restA restB
        go _ = pure False
  = checkOr $ go <$> splits
  | otherwise = pure False

checkArguments (KindSig ta ka : restA) (KindSig tb kb : restB)
  = checkMatch ta tb
 /\ checkMatch ka kb
 /\ checkArguments restA restB

checkArguments _ _ = pure False

