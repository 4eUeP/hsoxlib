module Sound.HSoxLib
  ( FFI.soxVersion
  , FFI.soxVersionInfo

  , FFI.withSox

  -- * Information
  , module Sound.HSoxLib.Information
  ) where

import qualified Sound.HSoxLib.FFI         as FFI
import           Sound.HSoxLib.Information
