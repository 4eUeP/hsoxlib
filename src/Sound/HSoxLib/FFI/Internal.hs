{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level interaction with underlying C API.
-- ( http://sox.sourceforge.net )
--
-- You don't want to use this, see "Sound.HSoxLib.FFI" instead. Or other more
-- higher level functions can be found in "Sound.HSoxLib".
module Sound.HSoxLib.FFI.Internal where

import qualified Foreign.C                    as C
import           Foreign.Ptr                  (FunPtr, Ptr)

import qualified Sound.HSoxLib.Types          as T
import qualified Sound.HSoxLib.Types.Internal as T

-------------------------------------------------------------------------------
-- * Misc

foreign import ccall unsafe "sox.h sox_version"
  c_sox_version :: C.CString

foreign import ccall unsafe "sox.h sox_version_info"
  c_sox_version_info :: Ptr T.SoxVersionInfo

foreign import ccall unsafe "sox.h sox_precision"
  c_sox_precision :: T.SoxEncoding -> C.CUInt -> C.CUInt

-------------------------------------------------------------------------------
-- * Read & Write

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
              -> IO C.CInt

-------------------------------------------------------------------------------
-- * File's metadata

foreign import ccall unsafe "sox.h sox_find_comment"
  c_sox_find_comment :: Ptr T.SoxComments -> C.CString -> IO C.CString

-------------------------------------------------------------------------------
-- * Effects

foreign import ccall unsafe "sox.h sox_create_effects_chain"
  c_sox_create_effects_chain :: Ptr T.SoxEncodinginfo
                             -> Ptr T.SoxEncodinginfo
                             -> IO (Ptr T.SoxEffectsChain)

foreign import ccall unsafe "sox.h sox_delete_effects_chain"
  c_sox_delete_effects_chain :: Ptr T.SoxEffectsChain -> IO ()

foreign import ccall unsafe "sox.h sox_add_effect"
  c_sox_add_effect :: Ptr T.SoxEffectsChain
                   -> Ptr T.SoxEffect
                   -> Ptr T.SoxSignalinfo
                   -> Ptr T.SoxSignalinfo
                   -> IO C.CInt

foreign import ccall unsafe "sox.h sox_flow_effects"
  c_sox_flow_effects :: Ptr T.SoxEffectsChain
                     -> FunPtr (T.SoxFlowEffectsCallback a)
                     -> Ptr a
                     -> IO C.CInt

foreign import ccall "sox.h sox_flow_effects"
  c_safe_sox_flow_effects :: Ptr T.SoxEffectsChain
                          -> FunPtr (T.SoxFlowEffectsCallback a)
                          -> Ptr a
                          -> IO C.CInt

foreign import ccall unsafe "wrapper"
  createFlowEffectsCallbackPtr :: T.SoxFlowEffectsCallback a
                               -> IO (FunPtr (T.SoxFlowEffectsCallback a))

foreign import ccall unsafe "sox.h sox_find_effect"
  c_sox_find_effect :: C.CString -> IO (Ptr T.SoxEffectHandler)

foreign import ccall unsafe "sox.h sox_create_effect"
  c_sox_create_effect :: Ptr T.SoxEffectHandler -> IO (Ptr T.SoxEffect)

foreign import ccall unsafe "sox.h sox_effect_options"
  c_sox_effect_options :: Ptr T.SoxEffect
                       -> C.CInt
                       -> Ptr C.CString
                       -> IO C.CInt

-------------------------------------------------------------------------------
-- * Local c functions under csrc directory

-- ** Custom effects

foreign import ccall unsafe "input_effect_fn0"
  c_input_effect_fn0 :: IO (Ptr T.SoxEffectHandler)

foreign import ccall unsafe "get_read_wide_samples"
  c_get_read_wide_samples :: IO T.SoxUInt64

foreign import ccall unsafe "get_input_read_time"
  c_get_input_read_time :: IO Double
