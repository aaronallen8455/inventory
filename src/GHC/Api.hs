{-# LANGUAGE CPP #-}
module GHC.Api
  ( module X
  ) where

#if __GLASGOW_HASKELL__ >= 900

import           GHC.Core.TyCo.Rep as X
import           GHC.Data.FastString as X
import           GHC.Driver.Session as X
import           GHC.Iface.Env as X
import           GHC.SysTools as X
import           GHC.Types.Name as X
import           GHC.Types.Name.Cache as X
import           GHC.Types.SrcLoc as X
import           GHC.Types.Unique.Supply as X
import           GHC.Utils.Misc as X

import           GHC.Iface.Ext.Binary as X
import           GHC.Iface.Ext.Types as X
import           GHC.Iface.Ext.Utils as X

#else

import           DynFlags as X
import           FastString as X
import           Name as X
import           NameCache as X
import           SrcLoc as X
import           SysTools as X
import           TyCoRep as X
import           UniqSupply as X
import           Util as X

import           HieBin as X
import           HieTypes as X
import           HieUtils as X

#endif
