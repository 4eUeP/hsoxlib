module Sound.HSoxLib
  ( FFI.soxVersion
  , FFI.soxVersionInfo

  , FFI.withSox

  -- * Information
  , module Sound.HSoxLib.Information

  -- * Effect
  , module Sound.HSoxLib.Effect
  ) where

import           Sound.HSoxLib.Effect
import qualified Sound.HSoxLib.FFI         as FFI
import           Sound.HSoxLib.Information
