module Sound.HSoxLib.Utils
  ( peekMaybeNull
  , peekCDoubleNull

  , maybePeekCString
  , peekCStringEmpty
  , peekCStringLenEmpty
  , pokeCStringWithTerm
  , pokeCString0

  , peekArrayCStrings
  , withCreateArray
  , makeCStringArray
  , makeCStringArray0
  , freeCStringArray
  , lengthArray0WithMax

  , calcTime
  , strTime
  ) where

import qualified Foreign.C            as C
import qualified Foreign.ForeignPtr   as P
import qualified Foreign.Marshal      as M
import           Foreign.Ptr          (Ptr, nullPtr)
import           Foreign.Storable     (Storable, peek, peekElemOff)
import           Text.Printf          (printf)

import qualified Data.Vector.Storable as SV

-- | Read a value from the given memory location.
--
-- If the location is @0@ (null pointer), then return 'Nothing'.
peekMaybeNull :: Storable a => Ptr a -> IO (Maybe a)
peekMaybeNull = M.maybePeek peek

-- | Marshal a pointer to C double into a Haskell Double.
--
-- If the pointer is NULL, then return 'Nothing'.
peekCDoubleNull :: Ptr C.CDouble -> IO (Maybe Double)
peekCDoubleNull ptr =
  maybeNullPeek Nothing ptr (fmap (Just . realToFrac) . peek)

-------------------------------------------------------------------------------

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

maybePeekCString :: C.CString -> IO (Maybe String)
maybePeekCString = M.maybePeek C.peekCString

-- | Poke string with a null terminator.
pokeCStringWithTerm :: C.CString -> String -> IO ()
pokeCStringWithTerm = pokeCString0 '\0'

pokeCString0 :: Char -> C.CString -> String -> IO ()
pokeCString0 marker ptr val = C.withCStringLen s f
  where
    s = val ++ [marker]
    f (p, l) = M.copyArray ptr p l

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
  M.peekArray l ptr >>= mapM_ M.free
  M.free ptr

-- | Like 'M.lengthArray0', but with a max length.
lengthArray0WithMax :: (Storable a, Eq a) => Int -> a -> Ptr a -> IO Int
lengthArray0WithMax maxLen marker ptr
  | maxLen <= 0 = return 0
  | otherwise = loop 0
  where
    loop i = do
      val <- peekElemOff ptr i
      if i >= maxLen
         then return maxLen
         else if val == marker then return i else loop (i + 1)

-------------------------------------------------------------------------------

maybeNullPeek :: a -> Ptr b -> (Ptr b -> IO a) -> IO a
maybeNullPeek defaultVal ptr peekfun | ptr == nullPtr = return defaultVal
                                     | otherwise = peekfun ptr

-------------------------------------------------------------------------------

-- | Transform time in seconds into tuple of
-- "(hours, minutes, seconds, milliseconds)".
--
-- >>> calcTime 360.1
-- (0,6,0,100)
--
-- >>> let totalSeconds = 3601.1234
-- >>> let (h, m, s, ms) = calcTime totalSeconds
-- >>> (h * 3600 + m * 60 + s) * 1000 + ms == (floor $ 3601.1234 * 1000)
-- True
calcTime :: (RealFloat a, Integral b) => a -> (b, b, b, b)
calcTime totalSeconds
  | totalSeconds <= 0 = (0, 0, 0, 0)
  | otherwise = let ts = floor totalSeconds
                    (hours, m) = ts `divMod` 3600
                    (mins, seconds) = m `divMod` 60
                    mseconds = floor (totalSeconds * 1000) - ts * 1000
                 in (hours, mins, seconds, mseconds)

-- | Show time(seconds) in "hour:minute:second:milliseconds" format.
--
-- Note: only two of characters will be printed.
--
-- >>> strTime 3723.004
-- "01:02:03.00"
-- >>> strTime 3723.04
-- "01:02:03.04"
strTime :: RealFloat a => a -> String
strTime totalSeconds =
  let h, m, s, ms :: Integer
      (h, m, s, ms) = calcTime totalSeconds
      s' :: Double
      s' = fromIntegral s + fromIntegral ms / 1000
   in printf "%02i:%02i:%05.2f" h m s'
