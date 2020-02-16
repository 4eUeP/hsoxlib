{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level interaction with underlying C API.
-- ( http://sox.sourceforge.net )
--
-- You don't want to use this, see "Sound.HSoxLib" instead.
module Sound.HSoxLib.FFI where

import qualified Foreign.C.String             as C
import qualified Foreign.C.Types              as C
import           Foreign.Ptr                  (Ptr)
import           Foreign.Storable             (peek)

import qualified Sound.HSoxLib.Types          as T
import qualified Sound.HSoxLib.Types.Internal as T
import qualified Sound.HSoxLib.Utils          as U

-------------------------------------------------------------------------------

-- | Return version number string of libsox, for example, "14.4.0".
soxVersion :: IO String
soxVersion = U.peekCStringEmpty c_sox_version

-- | Get information about this build of libsox.
soxVersionInfo :: IO T.SoxVersionInfo
soxVersionInfo = peek c_sox_version_info

-- | Initialize effects library.
--
-- Returns 'T.soxSuccess' if successful.
soxInit :: IO T.SoxError
soxInit = fmap T.SoxError c_sox_init

-- | Close effects library and unload format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxQuit :: IO T.SoxError
soxQuit = fmap T.SoxError c_sox_quit

-- | Find and load format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxFormatInit :: IO T.SoxError
soxFormatInit = fmap T.SoxError c_sox_format_init

-- | Unload format handler plugins.
soxFormatQuit :: IO ()
soxFormatQuit = c_sox_format_quit

-- | Given an encoding (for example, SIGN2) and the encoded bits_per_sample
-- (for example, 16), returns the number of useful bits per sample in the
-- decoded data (for example, 16), or returns 0 to indicate that the value
-- returned by the format handler should be used instead of a pre-determined
-- precision.
soxPrecision :: T.SoxEncoding -> C.CUInt -> Word
soxPrecision encoding bps = fromIntegral $ c_sox_precision encoding bps

-------------------------------------------------------------------------------

foreign import ccall unsafe "sox.h sox_version"
  c_sox_version :: C.CString

foreign import ccall unsafe "sox.h sox_version_info"
  c_sox_version_info :: Ptr T.SoxVersionInfo

foreign import ccall unsafe "sox.h sox_format_init"
  c_sox_format_init :: IO C.CInt

foreign import ccall unsafe "sox.h sox_format_quit"
  c_sox_format_quit :: IO ()

foreign import ccall unsafe "sox.h sox_init"
  c_sox_init :: IO C.CInt

foreign import ccall unsafe "sox.h sox_quit"
  c_sox_quit :: IO C.CInt

foreign import ccall unsafe "sox.h sox_precision"
  c_sox_precision :: T.SoxEncoding -> C.CUInt -> C.CUInt
