module Sound.HSoxLib.FFI where

import           Control.Exception          (bracket_)
import           Control.Monad              ((<=<))
import qualified Foreign.C                  as C
import qualified Foreign.ForeignPtr         as P
import qualified Foreign.Marshal            as M
import           Foreign.Ptr                (FunPtr, Ptr, nullFunPtr, nullPtr)
import           Foreign.Storable           (peek)

import qualified Data.Vector.Storable       as SV

import qualified Sound.HSoxLib.Internal.FFI as I
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
         -- ^ returns 'T.soxSuccess' if successful.
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
soxOpenRead fp sig enc ft =
  -- Note: here we can use 'C.withCString' to auto free the memory, because
  -- in libsox, sox_open_read will make a copy of these strings.
  C.withCString fp $ \cfp ->
    M.maybeWith C.withCString ft $ I.c_sox_open_read cfp sig enc

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
             -> Maybe String
             -- ^ Previously-determined file type,
             -- or 'Nothing' to auto-detect.
             -> Ptr T.SoxOOB
             -- ^ Out-of-band data to add to file, or NULL if none.
             -> FunPtr (T.CFilePath -> IO Bool)
             -- ^ Called if file exists to determine whether
             -- overwrite is ok. Can be NULL.
             -> IO (Ptr T.SoxFormat)
             -- ^ The new session handle, or null on failure.
soxOpenWrite fp sig enc ft oob f =
  -- Note: here we can use 'C.withCString' to auto free the memory, because
  -- in libsox, sox_open_write will make a copy of these strings.
  C.withCString fp $ \cfp ->
    M.maybeWith C.withCString ft $ \cft ->
      I.c_sox_open_write cfp sig enc cft oob f

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
-- * Effects

-- | Initializes an effects chain.
--
-- Returned handle must be closed with 'soxDeleteEffChain'.
soxCreateEffChain :: Ptr T.SoxEncodinginfo
                  -- ^ Input encoding.
                  -> Ptr T.SoxEncodinginfo
                  -- ^ Output encoding.
                  -> IO (Ptr T.SoxEffectsChain)
                  -- ^ Returned Handle, or null on failure.
soxCreateEffChain = I.c_sox_create_effects_chain

-- | Closes an effects chain.
soxDeleteEffChain :: Ptr T.SoxEffectsChain -> IO ()
soxDeleteEffChain = I.c_sox_delete_effects_chain

-- | Find the effect handler with the given name.
-- Return Effect pointer, or null if not found.
soxFindEffect :: String -> IO (Ptr T.SoxEffectHandler)
soxFindEffect "input0"  = I.c_input_effect_fn0
soxFindEffect "output0" = I.c_output_effect_fn0
soxFindEffect name      = C.withCString name I.c_sox_find_effect

-- | Create an effect using the given handler.
-- Return The new effect, or null if not found.
soxCreateEffect :: Ptr T.SoxEffectHandler -> IO (Ptr T.SoxEffect)
soxCreateEffect = I.c_sox_create_effect

-- Add an effect to the effects chain,
-- returns 'T.soxSuccess' if successful.
soxAddEffect :: Ptr T.SoxEffectsChain
             -- ^ Effects chain to which effect should be added.
             -> Ptr T.SoxEffect
             -- ^ Effect to be added.
             -> Ptr T.SoxSignalinfo
             -- ^ Input format.
             -> Ptr T.SoxSignalinfo
             -- ^ Output format.
             -> IO T.SoxError
soxAddEffect w x y z = fmap T.SoxError (I.c_sox_add_effect w x y z)

-- | Applies the command-line options to the effect.
soxEffectOptions :: Ptr T.SoxEffect
                 -- ^ Effect pointer on which to set options.
                 -> C.CInt
                 -- ^ Number of arguments in argv.
                 -> Ptr C.CString
                 -- ^ Array of command-line options.
                 -> IO C.CInt
                 -- ^ FIXME: the comment in sox.h say this is "the number of
                 -- arguments consumed", but the real is that this is a
                 -- sox_error_t, and will return SOX_SUCCESS if successful.
soxEffectOptions = I.c_sox_effect_options

-- | Runs the effects chain, returns 'T.soxSuccess' if successful.
soxFlowEffects :: Ptr T.SoxEffectsChain
               -- ^ Effects chain to run.
               -> IO T.SoxError
soxFlowEffects c = fmap T.SoxError (I.c_sox_flow_effects c nullFunPtr nullPtr)

-- | Runs the effects chain with a callback function,
-- returns 'T.soxSuccess' if successful.
soxFlowEffects0 :: Ptr T.SoxEffectsChain
               -- ^ Effects chain to run.
               -> FunPtr (T.SoxFlowEffectsCallback a)
               -- ^ Callback for monitoring flow progress.
               -> Ptr a
               -- ^ Data to pass into callback.
               -> IO T.SoxError
soxFlowEffects0 x y z = fmap T.SoxError (I.c_safe_sox_flow_effects x y z)

createFlowEffectsCallbackPtr :: T.SoxFlowEffectsCallback a
                             -> IO (FunPtr (T.SoxFlowEffectsCallback a))
createFlowEffectsCallbackPtr = I.create_flow_effects_callback

-- | Should be called with "input0" effect. Or you will always get 0.
getReadWideSamples :: IO T.SoxUInt64
getReadWideSamples = I.c_get_read_wide_samples

-- | Should be called with "input0" effect. Or you will always get 0.
getInputReadTime :: IO Double
getInputReadTime = I.c_get_input_read_time

-- | Should be called with "output0" effect. Or you will always get 0.
getVuMeterFst :: IO String
getVuMeterFst = C.peekCString =<< I.c_get_vu_meter_fst

-- | Should be called with "output0" effect. Or you will always get 0.
getVuMeterSnd :: IO String
getVuMeterSnd = C.peekCString =<< I.c_get_vu_meter_snd

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

-- | Check if the pointer is NULL, or an exception will be raised.
assertNotNull  :: String -> Ptr a -> IO (Ptr a)
assertNotNull name ptr | ptr == nullPtr = error $ name ++ " returned NULL."
                       | otherwise = return ptr

tryDevice :: String -> IO (Maybe String)
tryDevice ft = C.withCString ft $ U.maybePeekCString <=< I.c_try_device

deviceName :: String -> IO (Maybe String)
deviceName name = C.withCString name $ U.maybePeekCString <=< I.c_device_name
