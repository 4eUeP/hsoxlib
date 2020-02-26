{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level interaction with underlying C API.
-- ( http://sox.sourceforge.net )
--
-- You don't want to use this, see "Sound.HSoxLib" instead.
module Sound.HSoxLib.FFI where

import qualified Foreign.C.String             as C
import qualified Foreign.C.Types              as C
import qualified Foreign.ForeignPtr           as P
import           Foreign.Ptr                  (FunPtr, Ptr, nullPtr)
import           Foreign.Storable             (peek)

import qualified Data.Vector.Storable         as SV

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

-- | Opens a decoding session for a file.
-- Returned handle must be closed with 'soxClose'.
soxOpenRead :: FilePath
            -- ^ Path to file to be opened.
            -> Ptr T.SoxSignalinfo
            -- ^ Information already known about audio stream,
            -- or NULL if none.
            -> Ptr T.SoxEncodinginfo
            -- ^ Information already known about sample encoding,
            -- or NULL if none.
            -> Maybe String
            -- ^ Previously-determined file type, or 'Nothing' to auto-detect.
            -> IO (Ptr T.SoxFormat)
            -- ^ The handle for the new session, or NULL on failure.
soxOpenRead fp sig enc Nothing =
  C.withCString fp $ \cfp -> c_sox_open_read cfp sig enc nullPtr
soxOpenRead fp sig enc (Just ft) =
  C.withCString fp $ \cfp -> C.withCString ft $ c_sox_open_read cfp sig enc

-- | Opens an encoding session for a file.
-- Returned handle must be closed with 'soxClose'.
soxOpenWrite :: FilePath
             -- ^ Path to file to be written (required).
             -> Ptr T.SoxSignalinfo
             -- ^ Information about desired audio stream (required).
             -> Ptr T.SoxEncodinginfo
             -- ^ Information about desired sample encoding,
             -- or NULL to use defaults.
             -> T.CFileType
             -- ^ Previously-determined file type,
             -- or NULL to auto-detect.
             -> Ptr T.SoxOOB
             -- ^ Out-of-band data to add to file, or NULL if none.
             -> FunPtr (T.CFilePath -> IO Bool)
             -- ^ Called if file exists to determine whether
             -- overwrite is ok. Can be NULL.
             -> IO (Ptr T.SoxFormat)
             -- ^ The new session handle, or null on failure.
soxOpenWrite fp sig enc cft oob f =
  C.withCString fp $ \cfp -> c_sox_open_write cfp sig enc cft oob f

-- | Reads samples from a decoding session into a sample buffer.
-- Return a vector of samples with the number of samples decoded, or 0 for EOF.
soxRead :: Ptr T.SoxFormat -> IO (SV.Vector T.SoxSample, C.CSize)
soxRead fmtPtr = do
  fmt <- peek fmtPtr
  let len = T.sigLength $ T.fmtSignalinfo fmt
  case len of
    Nothing -> error "Unexpected length"
    Just n  -> if n < 0 then error $ "Unexpected length " ++ show n
                        else
                          -- FIXME: overflow ?
                          U.withCreateArray (fromIntegral n) $
                            \p -> c_sox_read fmtPtr p (fromIntegral n)

-- | Writes samples to an encoding session from a sample buffer.
soxWrite :: Ptr T.SoxFormat
         -- ^ Format pointer.
         -> SV.Vector T.SoxSample
         -- ^ Buffer from which to read samples.
         -> IO C.CSize
         -- ^ Number of samples encoded.
soxWrite fmtPtr vec =
  let (fptr, len) = SV.unsafeToForeignPtr0 vec
   in P.withForeignPtr fptr $ \ptr -> c_sox_write fmtPtr ptr (fromIntegral len)

-- | Closes an encoding or decoding session.
soxClose :: Ptr T.SoxFormat
         -- ^ Format pointer.
         -> IO T.SoxError
         -- ^ returns SOX_SUCCESS if successful.
soxClose = c_sox_close

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

foreign import ccall unsafe "sox.h sox_open_read"
  c_sox_open_read :: T.CFilePath
                  -> Ptr T.SoxSignalinfo
                  -> Ptr T.SoxEncodinginfo
                  -> T.CFileType
                  -> IO (Ptr T.SoxFormat)

foreign import ccall safe "sox.h sox_open_write"
  c_sox_open_write :: T.CFilePath
                   -> Ptr T.SoxSignalinfo
                   -> Ptr T.SoxEncodinginfo
                   -> T.CFileType
                   -> Ptr T.SoxOOB
                   -> FunPtr (T.CFilePath -> IO Bool)
                   -> IO (Ptr T.SoxFormat)

foreign import ccall unsafe "sox.h sox_read"
  c_sox_read :: Ptr T.SoxFormat
             -> Ptr T.SoxSample
             -> C.CSize
             -> IO C.CSize

foreign import ccall unsafe "sox.h sox_write"
  c_sox_write :: Ptr T.SoxFormat
              -> Ptr T.SoxSample
              -> C.CSize
              -> IO C.CSize

foreign import ccall unsafe "sox.h sox_close"
  c_sox_close :: Ptr T.SoxFormat
              -> IO T.SoxError

foreign import ccall unsafe "sox.h sox_precision"
  c_sox_precision :: T.SoxEncoding -> C.CUInt -> C.CUInt
