{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.HSoxLib.FFI.Internal where

import qualified Foreign.C                    as C
import           Foreign.Ptr                  (Ptr, FunPtr)

import qualified Sound.HSoxLib.Types          as T
import qualified Sound.HSoxLib.Types.Internal as T

-------------------------------------------------------------------------------
-- Effects

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

foreign import ccall unsafe "sox.h sox_find_effect"
  c_sox_find_effect :: C.CString -> Ptr T.SoxEffectHandler

foreign import ccall unsafe "sox.h sox_create_effect"
  c_sox_create_effect :: Ptr T.SoxEffectHandler -> IO (Ptr T.SoxEffect)

foreign import ccall unsafe "sox.h sox_effect_options"
  c_sox_effect_options :: Ptr T.SoxEffect
                       -> C.CInt
                       -> Ptr C.CString
                       -> IO C.CInt
