module Main (main) where

import           Foreign.Ptr         (Ptr, castPtr, freeHaskellFunPtr,
                                      nullFunPtr, nullPtr)
import           Foreign.Storable    (peek)
import           System.Environment  (getArgs)
import qualified System.IO           as SIO

import qualified Sound.HSoxLib       as Sox
import qualified Sound.HSoxLib.Types as T
import qualified Sound.HSoxLib.Utils as U

main :: IO ()
main = Sox.withSox $ do
  filepath <- head <$> getArgs
  play filepath

play :: FilePath -> IO ()
play filepath =
  Sox.simpleSoxOpenRead filepath $ \in' -> do
    let inSigPtr = T.getFmtSignalPtr in'
    simpleWrite inSigPtr $ \out' -> flowIO in' out'

-- Change "alsa" to use an alternative audio device driver
simpleWrite :: Ptr T.SoxSignalinfo -> (Ptr T.SoxFormat -> IO a) -> IO a
simpleWrite sig =
  Sox.withSoxOpenWrite "default" sig nullPtr (Just "alsa") nullPtr nullFunPtr

flowIO :: Ptr T.SoxFormat -> Ptr T.SoxFormat -> IO ()
flowIO in' out' = do
  let inEnc' = T.getFmtEncodingPtr in'
  let outEnc' = T.getFmtEncodingPtr out'
  let inSig' = T.getFmtSignalPtr in'
  let outSig' = T.getFmtSignalPtr out'

  -- FIXME: deep copy
  U.withCopy inSig' $ \intermSig' -> do
    inSig <- peek inSig'
    outSig <- peek outSig'

    Sox.withSoxCreateEffChain inEnc' outEnc' $ \chain -> do
      Sox.autoEffect0 chain intermSig' inSig' "input0" [castPtr in']

      if T.sigRate inSig /= T.sigRate outSig
         then Sox.autoEffect chain intermSig' outSig' "rate" 0 nullPtr
         else return ()

      if T.sigChannels inSig /= T.sigChannels outSig
         then Sox.autoEffect chain intermSig' outSig' "channels" 0 nullPtr
         else return ()

      Sox.autoEffect0 chain intermSig' outSig' "output0" [castPtr out']

      fn <- Sox.createFlowEffectsCallbackPtr flowEffectsCallback
      _ <- Sox.soxFlowEffects0 chain fn nullPtr
      freeHaskellFunPtr fn

flowEffectsCallback :: T.SoxFlowEffectsCallback a
flowEffectsCallback alldone _ = do
  readTime <- Sox.getInputReadTime
  vuMeterL <- Sox.getVuMeterFst
  vuMeterR <- Sox.getVuMeterSnd
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  SIO.hPutStr SIO.stdout $
    "\r=> " ++ U.strTime readTime ++ " " ++ vuMeterL ++ "|" ++ vuMeterR
  SIO.hPutStr SIO.stdout $ if alldone == T.soxTrue then "\n" else ""
  return $ T.soxSuccess
