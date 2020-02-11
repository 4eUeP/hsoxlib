{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.HSoxLib.Internal.FFI where

import qualified Foreign.C.String             as C
import qualified Foreign.C.Types              as C
import           Foreign.Ptr                  (Ptr)

import qualified Sound.HSoxLib.Internal.Types as T

-------------------------------------------------------------------------------

foreign import ccall unsafe "sox.h sox_version"
  c_sox_version :: C.CString

foreign import ccall unsafe "sox.h sox_version_info"
  c_sox_version_info :: Ptr T.SoxVersionInfo

foreign import ccall unsafe "sox.h sox_precision"
  c_sox_precision :: T.SoxEncoding -> C.CUInt -> C.CUInt
