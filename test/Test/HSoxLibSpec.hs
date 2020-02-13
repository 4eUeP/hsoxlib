module Test.HSoxLibSpec where

import           Test.Hspec

import           Sound.HSoxLib as SoxLib

spec :: Spec
spec = smoke

smoke :: Spec
smoke = describe "Smoke testing" $ do
  it "version of libsox should not be empty" $
    SoxLib.soxVersion >>= (`shouldSatisfy` (not . null))
