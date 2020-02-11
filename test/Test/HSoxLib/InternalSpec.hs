{-# LANGUAGE ForeignFunctionInterface #-}

module Test.HSoxLib.InternalSpec where

import qualified Foreign.C.String             as C
import qualified Foreign.C.Types              as C
import           Foreign.Ptr                  (Ptr, nullPtr)
import           Test.Hspec

import           Sound.HSoxLib.Internal.Utils as U

spec :: Spec
spec = do
  peekArrayCStringsSpec
  peekCStringEmptySpec
  peekCStringLenEmptySpec
  peekCDoubleNullSpec

foreign import ccall unsafe "cdata.h hello_cstring"
  hello_cstring :: C.CString

foreign import ccall unsafe "cdata.h hello_cstrings"
  hello_cstrings :: Ptr C.CString

foreign import ccall unsafe "cdata.h empty_cstrings"
  empty_cstrings :: Ptr C.CString

foreign import ccall unsafe "cdata.h cdouble_3_14"
  cdouble_3_14 :: Ptr C.CDouble

peekArrayCStringsSpec :: Spec
peekArrayCStringsSpec = describe "Utils.peekArrayCStrings" $ do
  it "hello" $
    U.peekArrayCStrings hello_cstrings `shouldReturn` ["hello", "world"]

  it "empty array" $
    U.peekArrayCStrings empty_cstrings `shouldReturn` []

  it "null pointer" $
    U.peekArrayCStrings nullPtr `shouldReturn` []


peekCStringEmptySpec :: Spec
peekCStringEmptySpec = describe "Utils.peekCStringEmpty" $ do
  it "hello" $
    U.peekCStringEmpty hello_cstring `shouldReturn` "hello"

  it "null pointer" $
    U.peekCStringEmpty nullPtr `shouldReturn` ""

peekCStringLenEmptySpec :: Spec
peekCStringLenEmptySpec = describe "Utils.peekCStringLenEmpty" $ do
  it "hello" $
    U.peekCStringLenEmpty 2 hello_cstring `shouldReturn` "he"

  it "null pointer" $
    U.peekCStringLenEmpty 2 nullPtr `shouldReturn` ""

peekCDoubleNullSpec :: Spec
peekCDoubleNullSpec = describe "Utils.peekCDoubleNull" $ do
  it "it just work" $
    U.peekCDoubleNull cdouble_3_14 `shouldReturn` Just (3.14 :: Double)

  it "null pointer" $
    U.peekCDoubleNull nullPtr `shouldReturn` Nothing
