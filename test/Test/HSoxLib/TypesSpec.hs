module Test.HSoxLib.TypesSpec where

import qualified Foreign.Marshal     as M
import           Foreign.Storable
import           Test.Hspec

import qualified Sound.HSoxLib.Types as T

spec :: Spec
spec = do
  peekpokeSoxSignalinfo
  peekpokeSoxInstrinfo
  peekpokeSoxLoopinfo
  peekpokeSoxOOB

aSignalinfo :: T.SoxSignalinfo
aSignalinfo =
  T.SoxSignalinfo { T.sigRate      = Just 44100
                  , T.sigChannels  = Just 1
                  , T.sigPrecision = Just 16
                  , T.sigLength    = Just 512
                  , T.sigMult      = Just 1
                  }

peekpokeSoxSignalinfo :: Spec
peekpokeSoxSignalinfo = describe "SoxSignalinfo" $ do
  it "peek/poke SoxSignalinfo" $ do
    p <- M.new T.defaultSoxSignalinfo
    peek p `shouldReturn` T.defaultSoxSignalinfo

    poke p aSignalinfo
    peek p `shouldReturn` aSignalinfo

    T.freeSoxSignalinfoMult0 p
    M.free p

aSoxInstrinfo :: T.SoxInstrinfo
aSoxInstrinfo =
  T.SoxInstrinfo { T.midiNote = 1
                 , T.midiLow  = 1
                 , T.midiHi   = 1
                 , T.loopMode = 1
                 , T.nloops   = 1
                 }

peekpokeSoxInstrinfo :: Spec
peekpokeSoxInstrinfo = describe "SoxInstrinfo" $ do
  it "peek/poke SoxInstrinfo" $ do
    M.with aSoxInstrinfo $ \p -> peek p `shouldReturn` aSoxInstrinfo

aSoxLoopinfo :: T.SoxLoopinfo
aSoxLoopinfo =
  T.SoxLoopinfo { T.loopStart  = 1
                , T.loopLength = 100
                , T.loopCount  = 1
                , T.loopType   = 0
                }

peekpokeSoxLoopinfo :: Spec
peekpokeSoxLoopinfo = describe "SoxLoopinfo" $ do
  it "peek/poke SoxLoopinfo" $ do
    M.with aSoxLoopinfo $ \p -> peek p `shouldReturn` aSoxLoopinfo

aSoxComments :: T.SoxComments
aSoxComments = ["key=value"]

aSoxOOB :: T.SoxOOB
aSoxOOB =
  T.SoxOOB { T.oobComments = aSoxComments
           , T.oobInstr    = Just aSoxInstrinfo
           , T.oobLoops    = [aSoxLoopinfo]
           }

peekpokeSoxOOB :: Spec
peekpokeSoxOOB = describe "SoxOOB" $ do
  it "peek/poke SoxOOB" $ do
    M.with aSoxOOB $ \p -> do
      peek p `shouldReturn` aSoxOOB

      T.freeSoxOOBComments0 p
      peek p `shouldReturn` aSoxOOB { T.oobComments = [] }

      T.freeSoxOOBInstr0 p
      peek p `shouldReturn` aSoxOOB { T.oobComments = []
                                    , T.oobInstr = Nothing
                                    }
