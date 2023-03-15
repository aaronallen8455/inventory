{-# LANGUAGE CPP #-}
module GHC.DynFlags
  ( baseDynFlags,
  ) where

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Data.Bool (OverridingBool(Always))
#endif
import           GHC.Api hiding (settings)
import           GHC.Paths (libdir)

#if !MIN_VERSION_ghc(9,6,0)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig =
#if __GLASGOW_HASKELL__ < 810
  ([], [])
#else
  LlvmConfig [] []
#endif
#endif

baseDynFlags :: IO DynFlags
baseDynFlags = do
  settings <- initSysTools libdir
  let dynFlags =
#if MIN_VERSION_ghc(9,6,0)
        defaultDynFlags settings
#else
        defaultDynFlags settings fakeLlvmConfig
#endif
  pure dynFlags { useColor = Always }

