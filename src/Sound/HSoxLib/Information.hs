module Sound.HSoxLib.Information
  ( soxVersion
  , soxVersionInfo

  , soxPrecision
  ) where

import qualified Foreign.C.Types     as C
import           Foreign.Storable    (peek)

import qualified Sound.HSoxLib.FFI   as I
import qualified Sound.HSoxLib.Types as T
import qualified Sound.HSoxLib.Utils as U

-- | Return version number string of libsox, for example, "14.4.0".
soxVersion :: IO String
soxVersion = U.peekCStringEmpty I.c_sox_version

-- | Get information about this build of libsox.
soxVersionInfo :: IO T.SoxVersionInfo
soxVersionInfo = peek I.c_sox_version_info

-- | Given an encoding (for example, SIGN2) and the encoded bits_per_sample
-- (for example, 16), returns the number of useful bits per sample in the
-- decoded data (for example, 16), or returns 0 to indicate that the value
-- returned by the format handler should be used instead of a pre-determined
-- precision.
soxPrecision :: T.SoxEncoding -> C.CUInt -> Word
soxPrecision encoding bps = fromIntegral $ I.c_sox_precision encoding bps
