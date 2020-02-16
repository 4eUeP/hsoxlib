module Sound.HSoxLib
  ( FFI.soxVersion
  , FFI.soxVersionInfo

  , withSox
  , withSox'
  , withSoxFormat'
  ) where

import           Control.Exception         (bracket_)

import qualified Sound.HSoxLib.FFI         as FFI
import qualified Sound.HSoxLib.Types       as T

-- | Initialization and cleanup.
-- All libsox actions should be enclosed in this 'withSox'.
withSox :: IO a -> IO a
withSox = bracket_ init' quit'
  where
    init' = let f = FFI.soxInit >>= assertSucc "soxInit"
                g = FFI.soxFormatInit >>= assertSucc "soxFormatInit"
             in f >> g
    quit' = FFI.soxQuit >>= assertSucc "soxQuit"

-- | Perform IO between 'soxInit' and 'soxQuit'.
--
-- If the return code from 'soxInit' and 'soxQuit' is not equal to
-- 'T.soxSuccess', an exception will be raised.
withSox' :: IO a -> IO a
withSox' = bracket_ init' quit'
  where
    init' = FFI.soxInit >>= assertSucc "soxInit"
    quit' = FFI.soxQuit >>= assertSucc "soxQuit"

-- | Perform IO between 'soxFormatInit' and 'soxFormatQuit'.
--
-- If the return code from 'soxFormatInit' is not equal to 'T.soxSuccess',
-- an exception will be raised.
withSoxFormat' :: IO a -> IO a
withSoxFormat' = bracket_ init' quit'
  where
    init' = FFI.soxFormatInit >>= assertSucc "soxFormatInit"
    quit' = FFI.soxFormatQuit

-------------------------------------------------------------------------------

assertSucc :: String -> T.SoxError -> IO ()
assertSucc name ret | ret == T.soxSuccess = return ()
                    | otherwise = error $ name ++ " failed: " ++ show ret
