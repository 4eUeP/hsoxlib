{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level interaction with underlying C API.
-- ( http://sox.sourceforge.net )
--
-- You don't want to use this, see "Sound.HSoxLib" instead.
module Sound.HSoxLib.FFI where

import qualified Foreign.C.String             as C
import qualified Foreign.C.Types              as C
import           Foreign.Ptr                  (Ptr)

import qualified Sound.HSoxLib.Types          as T
import qualified Sound.HSoxLib.Types.Internal as T

-------------------------------------------------------------------------------

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
