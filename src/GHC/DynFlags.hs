{-# OPTIONS_GHC -Wno-missing-fields #-}

-- Modified from ghc-lib-api-ext.

module GHC.DynFlags
  ( baseDynFlags,
  )
where

import           DynFlags hiding (settings)
import           GHC.Paths (libdir)
import           SysTools
import           Util

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

baseDynFlags :: IO DynFlags
baseDynFlags = do
  settings <- initSysTools libdir
  pure $ (defaultDynFlags settings fakeLlvmConfig)
    { useColor = Always }


