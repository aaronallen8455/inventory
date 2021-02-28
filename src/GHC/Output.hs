{-# LANGUAGE CPP #-}
module GHC.Output
  ( module X
  , Mode(..)
  ) where

#if __GLASGOW_HASKELL__ >= 900

import           GHC.Utils.Outputable as X
import           GHC.Utils.Ppr (Mode(..))
import           GHC.Utils.Ppr.Colour as X

#else

import           Outputable as X
import           PprColour as X
import           Pretty (Mode(..))

#endif
