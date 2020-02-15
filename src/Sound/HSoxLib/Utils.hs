module Sound.HSoxLib.Utils
  ( peekArrayCStrings
  , peekCStringEmpty
  , peekCStringLenEmpty
  , peekCDoubleNull
  , peekMaybeNull
  ) where

import qualified Foreign.C.String      as C
import qualified Foreign.C.Types       as C
import           Foreign.Marshal.Array (peekArray0)
import           Foreign.Ptr           (Ptr, nullPtr)
import           Foreign.Storable      (Storable, peek)

-- | Convert an array of C strings end with NULL pointer to haskell list.
-- For example, given @{"hello", "world", NULL}@ will get
-- @IO ["hello", "world"]@
--
-- /__Warning__: Array with no NULL in the end will produce unpredictable/
-- /result./
peekArrayCStrings :: Ptr C.CString -> IO [String]
peekArrayCStrings ptr = maybeNullPeek [] ptr peekfun
  where
    peekfun p = peekArray0 nullPtr p >>= mapM C.peekCString

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

-- | Read a value from the given memory location.
--
-- If the location is $0$ (null pointer), then return 'Nothing'.
peekMaybeNull :: Storable a => Ptr a -> IO (Maybe a)
peekMaybeNull ptr = maybeNullPeek Nothing ptr (fmap Just . peek)

-------------------------------------------------------------------------------

maybeNullPeek :: a -> Ptr b -> (Ptr b -> IO a) -> IO a
maybeNullPeek defaultVal ptr peekfun | ptr == nullPtr = return defaultVal
                                     | otherwise = peekfun ptr
