{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Do not use this module, see 'Sound.HSoxLib.Types' instead
module Sound.HSoxLib.Internal.Types where

#include <sox.h>

import           Data.Bits             ((.&.))
import           Data.Word
import qualified Foreign.C.Types       as C
import           Foreign.Storable

import qualified Sound.HSoxLib.Internal.Utils as U

-------------------------------------------------------------------------------

-- | Information about a build of libsox.
--
-- Some fields (for example, "versionExtra", "distro", "compiler") will
-- be treated as empty string if there is nothing about this information.
data SoxVersionInfo =
  SoxVersionInfo { versionFlags :: [SoxVersionFlag]
                 , versionCode  :: Word32
                 , version      :: String
                 , versionExtra :: String
                 , buildTime    :: String
                 , distro       :: String
                 , compiler     :: String
                 , arch         :: String
                 } deriving (Show)

instance Storable SoxVersionInfo where
  sizeOf _ = #{size sox_version_info_t}
  alignment _ = #{alignment sox_version_info_t}
  peek ptr =
    pure SoxVersionInfo
      <*> (fmap getFlags $ #{peek sox_version_info_t, flags} ptr)
      <*> #{peek_int sox_version_info_t, version_code} ptr
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, version} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, version_extra} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, time} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, distro} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, compiler} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_version_info_t, arch} ptr)
    where
      getFlags :: C.CInt -> [SoxVersionFlag]
      getFlags x
        | x == 0 = [soxFlagNone]
        | otherwise = filter (\y -> x .&. (soxVersionFlag y) /= 0) allFlags
      allFlags = [soxFlagPopen, soxFlagMagic, soxFlagThreads, soxFlagMemopen]
  poke = error "SoxVersionInfo.poke: NotImplemented."

-------------------------------------------------------------------------------

-- | Flags indicating whether optional features are present in this build
-- of libsox.
newtype SoxVersionFlag = SoxVersionFlag { soxVersionFlag :: C.CInt }
  deriving (Eq, Storable)

#{enum SoxVersionFlag, SoxVersionFlag
  , soxFlagNone    = sox_version_none
  , soxFlagPopen   = sox_version_have_popen
  , soxFlagMagic   = sox_version_have_magic
  , soxFlagThreads = sox_version_have_threads
  , soxFlagMemopen = sox_version_have_memopen
 }

instance Show SoxVersionFlag where
  show (SoxVersionFlag #{const sox_version_none})         = "none"
  show (SoxVersionFlag #{const sox_version_have_popen})   = "popen"
  show (SoxVersionFlag #{const sox_version_have_magic})   = "magic"
  show (SoxVersionFlag #{const sox_version_have_threads}) = "threads"
  show (SoxVersionFlag #{const sox_version_have_memopen}) = "memopen"
  show (SoxVersionFlag n) = error $ "libSoX: invalid version flag " ++ show n

-------------------------------------------------------------------------------
-- Macros

-- FIXME: this require gcc extension "typeof"
-- see: https://gcc.gnu.org/onlinedocs/gcc/Typeof.html
#define hsc_fieldtype(t, f) hsc_type(__typeof__ (((t *)0)->f))

#define hsc_peek_int(t, f)                                               \
  {                                                                      \
    hsc_printf(                                                          \
      "(\\hsc_ptr -> fmap fromIntegral (peekByteOff hsc_ptr %ld :: IO ", \
      (long) offsetof (t, f));                                           \
    hsc_fieldtype(t, f)                                                  \
    hsc_printf ("))");                                                   \
  }

#define hsc_poke_int(t, f)                                                   \
  {                                                                          \
    hsc_printf(                                                              \
      "(\\hsc_ptr hsc_x -> pokeByteOff hsc_ptr %ld (fromIntegral hsc_x :: ", \
      (long) offsetof (t, f));                                               \
    hsc_fieldtype(t, f)                                                      \
    hsc_printf("))");                                                        \
  }

#define hsc_peek_double(t, f)                                          \
  {                                                                    \
    hsc_printf(                                                        \
      "(\\hsc_ptr -> fmap realToFrac (peekByteOff hsc_ptr %ld :: IO ", \
      (long) offsetof (t, f));                                         \
    hsc_fieldtype(t, f)                                                \
    hsc_printf ("))");                                                 \
  }

#define hsc_poke_double(t, f)                                              \
  {                                                                        \
    hsc_printf(                                                            \
      "(\\hsc_ptr hsc_x -> pokeByteOff hsc_ptr %ld (realToFrac hsc_x :: ", \
      (long) offsetof (t, f));                                             \
    hsc_fieldtype(t, f)                                                    \
    hsc_printf ("))");                                                     \
  }
