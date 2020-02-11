-- | Low-level interaction with underlying C API.
-- ( http://sox.sourceforge.net )
--
-- You don't want to use this, see "Sound.HSoxLib" instead.
module Sound.HSoxLib.Internal
  ( module Sound.HSoxLib.Internal.FFI
  , module Sound.HSoxLib.Internal.Utils
  ) where

import           Sound.HSoxLib.Internal.FFI
import           Sound.HSoxLib.Internal.Utils
