module Sound.HSoxLib.Effect
  ( FFI.soxCreateEffChain
  , FFI.soxDeleteEffChain
  , FFI.soxFindEffect
  , FFI.soxCreateEffect
  , FFI.soxAddEffect
  , FFI.soxEffectOptions
  , FFI.soxFlowEffects
  , FFI.soxFlowEffects0

  -- * Composition

  , withSoxCreateEffChain
  , autoEffect
  , autoEffect'

  -- * Callbacks & Helpers

  , FFI.createFlowEffectsCallbackPtr
  , FFI.getReadWideSamples
  , FFI.getInputReadTime
  , FFI.getVuMeterFst
  , FFI.getVuMeterSnd
  ) where

import           Control.Exception   (bracket)
import qualified Foreign.C           as C
import qualified Foreign.Marshal     as M
import           Foreign.Ptr         (Ptr)

import qualified Sound.HSoxLib.FFI   as FFI
import qualified Sound.HSoxLib.Types as T

-- | Like 'FFI.soxCreateEffChain', but you don't need to manual delete the
-- effect chain. If the returned effect chain is null, then raise an exception.
withSoxCreateEffChain :: Ptr T.SoxEncodinginfo
                      -> Ptr T.SoxEncodinginfo
                      -> (Ptr T.SoxEffectsChain -> IO a)
                      -> IO a
withSoxCreateEffChain i o = bracket init' FFI.soxDeleteEffChain
  where
    init' = FFI.soxCreateEffChain i o >>= FFI.assertNotNull "soxCreateEffChain"

-- | Apply effect to effect chain.
autoEffect :: Ptr T.SoxEffectsChain
            -- ^ Effects chain to which effect should be added.
            -> Ptr T.SoxSignalinfo
            -- ^ Input format.
            -> Ptr T.SoxSignalinfo
            -- ^ Output format.
            -> String
            -- ^ Effect name
            -> C.CInt
            -- ^ Number of arguments in the options.
            -> Ptr C.CString
            -- ^ Array of command-line options.
            -> IO ()
autoEffect chain i o name l poptions = bracket init' quit' action
  where
    init' = g =<< FFI.soxCreateEffect =<< f =<< FFI.soxFindEffect name
    quit' = M.free
    action e = do
      _ <- w . T.SoxError =<< FFI.soxEffectOptions e l poptions
      _ <- h =<< FFI.soxAddEffect chain e i o
      return ()
    f = FFI.assertNotNull "soxFindEffect"
    g = FFI.assertNotNull "soxCreateEffect"
    h = FFI.assertSucc "soxAddEffect"
    w = FFI.assertSucc "soxEffectOptions"

autoEffect' :: Ptr T.SoxEffectsChain
             -- ^ Effects chain to which effect should be added.
             -> Ptr T.SoxSignalinfo
             -- ^ Input format.
             -> Ptr T.SoxSignalinfo
             -- ^ Output format.
             -> String
             -- ^ Effect name
             -> [C.CString]
             -- ^ Array of command-line options.
             -> IO ()
autoEffect' chain i o name options =
  let l = fromIntegral $ length options
   in M.withArray options $ autoEffect chain i o name l
