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
