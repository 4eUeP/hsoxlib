module Sound.HSoxLib.Information
  ( soxVersion
  , soxVersionInfo
  ) where

import           Foreign.Storable             (peek)

import qualified Sound.HSoxLib.Internal.FFI   as I
import qualified Sound.HSoxLib.Internal.Utils as I
import qualified Sound.HSoxLib.Types          as T

-- | Return version number string of libsox, for example, "14.4.0".
soxVersion :: IO String
soxVersion = I.peekCStringEmpty I.c_sox_version

-- | Get information about this build of libsox.
soxVersionInfo :: IO T.SoxVersionInfo
soxVersionInfo = peek I.c_sox_version_info
