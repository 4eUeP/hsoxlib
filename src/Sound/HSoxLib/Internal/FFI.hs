{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.HSoxLib.Internal.FFI where

import qualified Foreign.C.String             as C
import           Foreign.Ptr                  (Ptr)

import qualified Sound.HSoxLib.Internal.Types as T

-------------------------------------------------------------------------------

foreign import ccall unsafe "sox.h sox_version"
  c_sox_version :: C.CString

foreign import ccall unsafe "sox.h sox_version_info"
  c_sox_version_info :: Ptr T.SoxVersionInfo
