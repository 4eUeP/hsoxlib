module Sound.HSoxLib.Information
  ( I.SoxVersionInfo (..)
  , I.SoxVersionFlag
  , soxVersion
  , soxVersionInfo
  ) where

import           Foreign.Storable             (peek)

import qualified Sound.HSoxLib.Internal       as I


-- | Return version number string of libsox, for example, "14.4.0".
soxVersion :: IO String
soxVersion = I.peekCStringEmpty I.c_sox_version

-- | Get information about this build of libsox.
soxVersionInfo :: IO I.SoxVersionInfo
soxVersionInfo = peek I.c_sox_version_info
