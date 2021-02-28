{-# LANGUAGE CPP #-}
module GHC.DynFlags
  ( baseDynFlags,
  ) where

import           GHC.Api hiding (settings)
import           GHC.Paths (libdir)

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig =
#if __GLASGOW_HASKELL__ < 810
  ([], [])
#else
  LlvmConfig [] []
#endif

baseDynFlags :: IO DynFlags
baseDynFlags = do
  settings <- initSysTools libdir
  pure $ (defaultDynFlags settings fakeLlvmConfig)
    { useColor = Always }

