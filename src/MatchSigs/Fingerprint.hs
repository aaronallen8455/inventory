{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module MatchSigs.Fingerprint
  ( SigFingerprint(..)
  , sigFingerprint
  ) where

import           Control.Monad.State
import qualified Data.IntMap.Strict as IM
import           Data.List
import qualified Data.Map.Strict as M

import           Name
import           MatchSigs.Sig (FreeVarIdx, Sig(..))
import           FastString


-- | Used to check if the result types of two functions are similar enough to
-- warrant deeper comparison
data TypeFingerprint varIx
  = TFFree !varIx
  | TFName !FastString !(Maybe Name)
  | TFApp !(TypeFingerprint varIx) ![TypeFingerprint varIx]
  | TFFun ![TypeFingerprint varIx]
  | TFTuple ![TypeFingerprint ()]
    -- we have to ignore vars completely in case of tuples because there's no
    -- totally reliable way to sort the elements in the presence of free vars
  | TFKindSig !(TypeFingerprint varIx) !(TypeFingerprint varIx)
  deriving (Eq, Ord, Functor)

instance Show varIx => Show (TypeFingerprint varIx) where
  show (TFFree i) = show i
  show (TFName _ _) = "name"
  show (TFApp fp fps) = "app " <> show fp <> unwords (show <$> fps)
  show (TFFun fps) = "fun " <> unwords (show <$> fps)
  show (TFTuple fps) = "tuple " <> unwords (show <$> fps)
  show (TFKindSig ty ki) = "kind sig " <> show ty
                        <> " :: " <> show ki

-- | Using this as a Map key provides a heuristic for matching only the
-- signatures that share key characteristics rather than blindly comparing
-- every signature.
data SigFingerprint =
  SF { sfArgs     :: !Int -- the length of the [Sig]
     , sfFreeVars :: ![(Int, Int)] -- num occurances, num of vars that occur that many times
     , sfResult   :: !(TypeFingerprint FreeVarIdx)
     } deriving (Eq, Ord)

-- | Produce a 'SigFingerprint' from the result type of a signature
sigFingerprint :: [Sig FreeVarIdx] -> SigFingerprint
sigFingerprint sig
  | r : _ <- reverse sig
  = SF { sfArgs = length sig
       , sfFreeVars = sigFreeVars sig
       , sfResult = resultFingerprint r
       }
  | otherwise = error "empty Sig"

-- | Turn the 'Sig' into it's 'TypeFingerprint' representation
resultFingerprint :: Sig FreeVarIdx -> TypeFingerprint FreeVarIdx
resultFingerprint s = evalState (go s) IM.empty
  where
    go :: Sig FreeVarIdx -> State (IM.IntMap FreeVarIdx) (TypeFingerprint FreeVarIdx)
    go = \case
      TyDescriptor str mbName -> pure $ TFName str mbName
      FreeVar i  ->
        do m <- get
           case m IM.!? i of
             Nothing -> let i' = IM.size m
                         in TFFree i' <$ modify' (IM.insert i i')
             Just i' -> pure $ TFFree i'
      Arg ss     -> TFFun <$> traverse go ss
      Qual _     -> pure $ TFFun [] -- ignore quals in result type
      Apply c as -> TFApp <$> (TFFun <$> traverse go c)
                          <*> traverse (fmap TFFun . traverse go) as
      VarCtx _   -> pure $ TFFun []
      Tuple xs   -> TFTuple . sort . map void
                <$> traverse (fmap TFFun . traverse go) xs
      KindSig ty ki -> TFKindSig <$> (TFFun <$> traverse go ty)
                                 <*> (TFFun <$> traverse go ki)

-- | An ordering heuristic based on the count of vars with a certain number
-- of occurrences in the signature.
sigFreeVars :: [Sig FreeVarIdx] -> [(Int, Int)] -- Num occurrances <=> num of vars
sigFreeVars = M.toList
            . foldMap (`M.singleton` 1)
            . (foldMap . foldMap) (`M.singleton` 1) -- occurances of each var
