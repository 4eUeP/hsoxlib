module Sound.HSoxLib.Utils
  ( peekMaybeNull
  , peekCDoubleNull

  , peekCStringEmpty
  , peekCStringLenEmpty
  , pokeCStringWithTerm
  , pokeCString0

  , fromMaybeNew

  , peekArrayCStrings
  , withCreateArray
  , makeCStringArray
  , makeCStringArray0
  , freeCStringArray
  ) where

import qualified Foreign.C            as C
import qualified Foreign.ForeignPtr   as P
import qualified Foreign.Marshal      as M
import           Foreign.Ptr          (Ptr, nullPtr)
import           Foreign.Storable     (Storable, peek)

import qualified Data.Vector.Storable as SV

-- | The same as 'C.peekCString', with this function's input can be a
-- null pointer.
peekCStringEmpty :: C.CString -> IO String
peekCStringEmpty ptr = maybeNullPeek "" ptr C.peekCString

-- | Marshal a C string with explicit length into a Haskell string,
-- with the given C string can be a null pointer.
peekCStringLenEmpty :: Int -> C.CString -> IO String
peekCStringLenEmpty len ptr = maybeNullPeek "" ptr peekfun
  where
    peekfun = (flip . curry $ C.peekCStringLen) len

-- | Marshal a pointer to C double into a Haskell Double.
--
-- If the pointer is NULL, then return 'Nothing'.
peekCDoubleNull :: Ptr C.CDouble -> IO (Maybe Double)
peekCDoubleNull ptr =
  maybeNullPeek Nothing ptr (fmap (Just . realToFrac) . peek)

-- | Poke string with a null terminator.
pokeCStringWithTerm :: C.CString -> String -> IO ()
pokeCStringWithTerm = pokeCString0 '\0'

pokeCString0 :: Char -> C.CString -> String -> IO ()
pokeCString0 marker ptr val = C.withCStringLen s f
  where
    s = val ++ [marker]
    f (p, l) = M.copyArray ptr p l

-- | Read a value from the given memory location.
--
-- If the location is $0$ (null pointer), then return 'Nothing'.
peekMaybeNull :: Storable a => Ptr a -> IO (Maybe a)
peekMaybeNull ptr = maybeNullPeek Nothing ptr (fmap Just . peek)

-------------------------------------------------------------------------------

-- | Like 'M.new', but can given 'Nothing' (will get 'nullPtr').
fromMaybeNew :: Storable a => Maybe a -> IO (Ptr a)
fromMaybeNew Nothing  = return nullPtr
fromMaybeNew (Just x) = M.new x

-------------------------------------------------------------------------------
-- Array

-- | Convert an array of C strings end with NULL pointer to haskell list.
-- For example, given @{"hello", "world", NULL}@ will get
-- @IO ["hello", "world"]@
--
-- /Array with no NULL in the end may produce unpredictable result./
peekArrayCStrings :: Ptr C.CString -> IO [String]
peekArrayCStrings ptr = maybeNullPeek [] ptr peekfun
  where
    peekfun p = M.peekArray0 nullPtr p >>= mapM C.peekCString

withCreateArray :: Storable a => Int -> (Ptr a -> IO r) -> IO (SV.Vector a, r)
withCreateArray l f = do
  fptr <- P.mallocForeignPtrArray l
  P.withForeignPtr fptr $ \p -> do
    r <- f p
    return (SV.unsafeFromForeignPtr0 fptr l, r)

-- | Write the list of strings consecutive into memory.
-- Must be explicitly freed using 'M.free' or 'freeCStringArray'.
makeCStringArray :: [String] -> IO (Ptr C.CString)
makeCStringArray xs = M.newArray =<< mapM C.newCString xs

-- | Like 'makeCStringArray', but with an extra position to hold a special
-- termination element.
makeCStringArray0 :: C.CString -> [String] -> IO (Ptr C.CString)
makeCStringArray0 marker xs = M.newArray0 marker =<< mapM C.newCString xs

freeCStringArray :: Int -> Ptr C.CString -> IO ()
freeCStringArray l ptr = do
  cs <- M.peekArray l ptr
  mapM_ M.free cs
  M.free ptr

-------------------------------------------------------------------------------

maybeNullPeek :: a -> Ptr b -> (Ptr b -> IO a) -> IO a
maybeNullPeek defaultVal ptr peekfun | ptr == nullPtr = return defaultVal
                                     | otherwise = peekfun ptr
