module Sound.HSoxLib
  ( soxInit
  , soxQuit
  , soxFormatInit
  , soxFormatQuit

  , module Sound.HSoxLib.Information
  ) where

import           Sound.HSoxLib.Information

import qualified Sound.HSoxLib.Internal.FFI as I
import qualified Sound.HSoxLib.Types        as T

-- | Initialize effects library.
--
-- Returns 'T.soxSuccess' if successful.
soxInit :: IO T.SoxError
soxInit = fmap T.SoxError I.c_sox_init

-- | Close effects library and unload format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxQuit :: IO T.SoxError
soxQuit = fmap T.SoxError I.c_sox_quit

-- | Find and load format handler plugins.
--
-- Returns 'T.soxSuccess' if successful.
soxFormatInit :: IO T.SoxError
soxFormatInit = fmap T.SoxError I.c_sox_format_init

-- | Unload format handler plugins.
soxFormatQuit :: IO ()
soxFormatQuit = I.c_sox_format_quit
