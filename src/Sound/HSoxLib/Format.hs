-- | API for using libSoX file formats.
module Sound.HSoxLib.Format
  ( FFI.soxOpenRead
  , FFI.soxRead
  , FFI.soxOpenWrite
  , FFI.soxWrite

  , withSoxOpenRead
  , simpleSoxOpenRead
  , withSoxOpenWrite
  , simpleSoxOpenWrite

  ) where

import           Control.Exception   (bracket)
import           Foreign.Ptr         (FunPtr, Ptr, nullFunPtr, nullPtr)

import qualified Sound.HSoxLib.FFI   as FFI
import qualified Sound.HSoxLib.Types as T

-- | Like 'soxOpenRead', with auto close the returned handle.
--
-- If the handle is null pointer, raise an exception.
withSoxOpenRead :: FilePath
                -> Ptr T.SoxSignalinfo
                -> Ptr T.SoxEncodinginfo
                -> Maybe String
                -> (Ptr T.SoxFormat -> IO a)
                -> IO a
withSoxOpenRead fp sig enc ft = bracket init' FFI.soxClose
  where
    init' = FFI.soxOpenRead fp sig enc ft >>= FFI.assertNotNull "soxOpenRead"

-- | Like 'withSoxOpenRead', but with all default options.
simpleSoxOpenRead :: FilePath -> (Ptr T.SoxFormat -> IO a) -> IO a
simpleSoxOpenRead fp = withSoxOpenRead fp nullPtr nullPtr Nothing

-- | Like 'FFI.soxOpenWrite', with auto close the returned handle.
--
-- If the handle is null pointer, raise an exception.
withSoxOpenWrite :: FilePath
                 -> Ptr T.SoxSignalinfo
                 -> Ptr T.SoxEncodinginfo
                 -> Maybe String
                 -> Ptr T.SoxOOB
                 -> FunPtr (T.CFilePath -> IO Bool)
                 -> (Ptr T.SoxFormat -> IO a)
                 -> IO a
withSoxOpenWrite fp sig enc ft oob func = bracket init' FFI.soxClose
  where
    init' = let m = FFI.soxOpenWrite fp sig enc ft oob func
             in m >>= FFI.assertNotNull "soxOpenWrite"

-- | OpenWrite with default options.
simpleSoxOpenWrite :: FilePath
                   -> Ptr T.SoxSignalinfo
                   -> (Ptr T.SoxFormat -> IO a)
                   -> IO a
simpleSoxOpenWrite fp p =
  withSoxOpenWrite fp p nullPtr Nothing nullPtr nullFunPtr
