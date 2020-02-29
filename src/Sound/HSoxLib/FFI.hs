module Sound.HSoxLib.FFI where

import           Control.Exception          (bracket, bracket_)
import           Control.Monad              ((<=<))
import qualified Foreign.C.String           as C
import qualified Foreign.C.Types            as C
import qualified Foreign.ForeignPtr         as P
import           Foreign.Ptr                (FunPtr, Ptr, nullFunPtr, nullPtr)
import           Foreign.Storable           (peek)

import qualified Data.Vector.Storable       as SV

import qualified Sound.HSoxLib.FFI.Internal as I
import qualified Sound.HSoxLib.Types        as T
import qualified Sound.HSoxLib.Utils        as U

-------------------------------------------------------------------------------
-- * Init & Quit

-- | Initialize effects library.
--
-- Returns 'T.soxSuccess' if successful.
soxInit :: IO T.SoxError
soxInit = fmap T.SoxError I.c_sox_init

-- | Close effects library and unload format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxQuit :: IO T.SoxError
soxQuit = fmap T.SoxError I.c_sox_quit

-- | Perform IO between 'soxInit' and 'soxQuit'.
--
-- If the return code from 'soxInit' and 'soxQuit' is not equal to
-- 'T.soxSuccess', an exception will be raised.
withSox' :: IO a -> IO a
withSox' = bracket_ init' quit'
  where
    init' = soxInit >>= assertSucc "soxInit"
    quit' = soxQuit >>= assertSucc "soxQuit"

-- | Find and load format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxFormatInit :: IO T.SoxError
soxFormatInit = fmap T.SoxError I.c_sox_format_init

-- | Unload format handler plugins.
soxFormatQuit :: IO ()
soxFormatQuit = I.c_sox_format_quit

-- | Perform IO between 'soxFormatInit' and 'soxFormatQuit'.
--
-- If the return code from 'soxFormatInit' is not equal to 'T.soxSuccess',
-- an exception will be raised.
withSoxFormat' :: IO a -> IO a
withSoxFormat' = bracket_ init' quit'
  where
    init' = soxFormatInit >>= assertSucc "soxFormatInit"
    quit' = soxFormatQuit

-- | Initialization and cleanup.
-- All libsox actions should be enclosed in this 'withSox'.
withSox :: IO a -> IO a
withSox = bracket_ init' quit'
  where
    init' = let f = soxInit >>= assertSucc "soxInit"
                g = soxFormatInit >>= assertSucc "soxFormatInit"
             in f >> g
    quit' = soxQuit >>= assertSucc "soxQuit"

-- | Closes an encoding or decoding session.
soxClose :: Ptr T.SoxFormat
         -- ^ Format pointer.
         -> IO T.SoxError
         -- ^ returns SOX_SUCCESS if successful.
soxClose = fmap T.SoxError . I.c_sox_close

-------------------------------------------------------------------------------
-- * Read

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
  C.withCString fp $ \cfp -> I.c_sox_open_read cfp sig enc nullPtr
soxOpenRead fp sig enc (Just ft) =
  C.withCString fp $ \cfp -> C.withCString ft $ I.c_sox_open_read cfp sig enc

-- | Like 'soxOpenRead', with auto close the returned handle.
--
-- If the handle is null pointer, raise an exception.
withSoxOpenRead :: FilePath
                -> Ptr T.SoxSignalinfo
                -> Ptr T.SoxEncodinginfo
                -> Maybe String
                -> (Ptr T.SoxFormat -> IO a)
                -> IO a
withSoxOpenRead fp sig enc ft = bracket init' soxClose
  where
    init' = soxOpenRead fp sig enc ft >>= assertHandle "soxOpenRead"

-- | Like 'withSoxOpenRead', but with all default options.
simpleSoxOpenRead :: FilePath -> (Ptr T.SoxFormat -> IO a) -> IO a
simpleSoxOpenRead fp = withSoxOpenRead fp nullPtr nullPtr Nothing

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
                            \p -> I.c_sox_read fmtPtr p (fromIntegral n)

-- | Find file's metadata block by a key, ignoring case.
-- If "key=value" is found, return value, else return 'Nothing'.
soxFindComment :: Ptr T.SoxComments -> String -> IO (Maybe String)
soxFindComment ptr key =
  C.withCString key $ U.maybePeekCString <=< I.c_sox_find_comment ptr

-- | Like 'soxFindComment', with an extra function to parse the result.
soxReadComments :: Ptr T.SoxComments
                -> (String -> Maybe a)
                -- ^ Functin to parse the value that returned from
                -- 'soxFindComment'.
                -> String
                -- ^ Key passed to 'I.soxFindComment'.
                -> IO (Maybe a)
soxReadComments ptr rd key = do
  mval <- soxFindComment ptr key
  return $ mval >>= rd

-------------------------------------------------------------------------------
-- * Write

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
  C.withCString fp $ \cfp -> I.c_sox_open_write cfp sig enc cft oob f

-- | Like 'soxOpenWrite', with auto close the returned handle.
--
-- If the handle is null pointer, raise an exception.
withSoxOpenWrite :: FilePath
                 -> Ptr T.SoxSignalinfo
                 -> Ptr T.SoxEncodinginfo
                 -> T.CFileType
                 -> Ptr T.SoxOOB
                 -> FunPtr (T.CFilePath -> IO Bool)
                 -> (Ptr T.SoxFormat -> IO a)
                 -> IO a
withSoxOpenWrite fp sig enc ft oob func = bracket init' soxClose
  where
    init' = soxOpenWrite fp sig enc ft oob func >>= assertHandle "soxOpenWrite"

-- | OpenWrite with default options.
simpleSoxOpenWrite :: FilePath
                   -> Ptr T.SoxSignalinfo
                   -> (Ptr T.SoxFormat -> IO a)
                   -> IO a
simpleSoxOpenWrite fp p =
  withSoxOpenWrite fp p nullPtr nullPtr nullPtr nullFunPtr

-- | Writes samples to an encoding session from a sample buffer.
soxWrite :: Ptr T.SoxFormat
         -- ^ Format pointer.
         -> SV.Vector T.SoxSample
         -- ^ Buffer from which to read samples.
         -> IO C.CSize
         -- ^ Number of samples encoded.
soxWrite fmtPtr vec =
  let (fptr, len) = SV.unsafeToForeignPtr0 vec
   in P.withForeignPtr fptr $ \ptr ->
     I.c_sox_write fmtPtr ptr (fromIntegral len)

-------------------------------------------------------------------------------
-- * Misc

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

-------------------------------------------------------------------------------
-- * Helpers

-- | Check if the function's return code is 'T.soxSuccess'. Or an exception
-- will be raised.
assertSucc :: String
           -- ^ Function name. Used as information to print if the return code
           -- is not 'T.soxSuccess'.
           -> T.SoxError
           -- ^ Function's return code.
           -> IO T.SoxError
assertSucc name ret | ret == T.soxSuccess = return ret
                    | otherwise = error $ name ++ " failed: " ++ show ret

-- | Check if the the handle returned by 'soxOpenRead' and 'soxOpenWrite' is
-- NULL. Or an exception will be raised.
assertHandle :: String -> Ptr a -> IO (Ptr a)
assertHandle name ptr | ptr == nullPtr = error $ name ++ " returned NULL."
                      | otherwise = return ptr
