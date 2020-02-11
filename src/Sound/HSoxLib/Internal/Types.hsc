{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Do not use this module, see 'Sound.HSoxLib.Types' instead
module Sound.HSoxLib.Internal.Types where

#include <sox.h>

import           Data.Bits                    ((.&.))
import           Data.Word
import qualified Foreign.C.Types              as C
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

-- | Format of sample data.
newtype SoxEncoding = SoxEncoding #{type sox_encoding_t}
  deriving (Eq, Storable)

#{enum SoxEncoding, SoxEncoding
  , encodingUnknown   = SOX_ENCODING_UNKNOWN
  , encodingSign2     = SOX_ENCODING_SIGN2
  , encodingUnsigned  = SOX_ENCODING_UNSIGNED
  , encodingFloat     = SOX_ENCODING_FLOAT
  , encodingFloatText = SOX_ENCODING_FLOAT_TEXT
  , encodingFlac      = SOX_ENCODING_FLAC
  , encodingHcom      = SOX_ENCODING_HCOM
  , encodingWavpack   = SOX_ENCODING_WAVPACK
  , encodingWavpackf  = SOX_ENCODING_WAVPACKF
  , encodingUlaw      = SOX_ENCODING_ULAW
  , encodingAlaw      = SOX_ENCODING_ALAW
  , encodingG721      = SOX_ENCODING_G721
  , encodingG723      = SOX_ENCODING_G723
  , encodingClADPCM   = SOX_ENCODING_CL_ADPCM
  , encodingClADPCM16 = SOX_ENCODING_CL_ADPCM16
  , encodingMsADPCM   = SOX_ENCODING_MS_ADPCM
  , encodingImaADPCM  = SOX_ENCODING_IMA_ADPCM
  , encodingOkiADPCM  = SOX_ENCODING_OKI_ADPCM
  , encodingDPCM      = SOX_ENCODING_DPCM
  , encodingDWVW      = SOX_ENCODING_DWVW
  , encodingDWVWN     = SOX_ENCODING_DWVWN
  , encodingGSM       = SOX_ENCODING_GSM
  , encodingMP3       = SOX_ENCODING_MP3
  , encodingVorbis    = SOX_ENCODING_VORBIS
  , encodingAmrWB     = SOX_ENCODING_AMR_WB
  , encodingAmrNB     = SOX_ENCODING_AMR_NB
  , encodingCVSD      = SOX_ENCODING_CVSD
  , encodingLPC10     = SOX_ENCODING_LPC10
  , encodingOpus      = SOX_ENCODING_OPUS
 }

soxEncodingsLen :: Int
soxEncodingsLen = #{const SOX_ENCODINGS}

instance Bounded SoxEncoding where
  minBound = SoxEncoding #{const SOX_ENCODING_UNKNOWN}
  maxBound = SoxEncoding $ pred #{const SOX_ENCODINGS}

instance Show SoxEncoding where
  show (SoxEncoding #{const SOX_ENCODING_UNKNOWN})    = "encodingUnknown"
  show (SoxEncoding #{const SOX_ENCODING_SIGN2})      = "encodingSign2"
  show (SoxEncoding #{const SOX_ENCODING_UNSIGNED})   = "encodingUnsigned"
  show (SoxEncoding #{const SOX_ENCODING_FLOAT})      = "encodingFloat"
  show (SoxEncoding #{const SOX_ENCODING_FLOAT_TEXT}) = "encodingFloatText"
  show (SoxEncoding #{const SOX_ENCODING_FLAC})       = "encodingFlac"
  show (SoxEncoding #{const SOX_ENCODING_HCOM})       = "encodingHcom"
  show (SoxEncoding #{const SOX_ENCODING_WAVPACK})    = "encodingWavpack"
  show (SoxEncoding #{const SOX_ENCODING_WAVPACKF})   = "encodingWavpackf"
  show (SoxEncoding #{const SOX_ENCODING_ULAW})       = "encodingUlaw"
  show (SoxEncoding #{const SOX_ENCODING_ALAW})       = "encodingAlaw"
  show (SoxEncoding #{const SOX_ENCODING_G721})       = "encodingG721"
  show (SoxEncoding #{const SOX_ENCODING_G723})       = "encodingG723"
  show (SoxEncoding #{const SOX_ENCODING_CL_ADPCM})   = "encodingClADPCM"
  show (SoxEncoding #{const SOX_ENCODING_CL_ADPCM16}) = "encodingClADPCM16"
  show (SoxEncoding #{const SOX_ENCODING_MS_ADPCM})   = "encodingMsADPCM"
  show (SoxEncoding #{const SOX_ENCODING_IMA_ADPCM})  = "encodingImaADPCM"
  show (SoxEncoding #{const SOX_ENCODING_OKI_ADPCM})  = "encodingOkiADPCM"
  show (SoxEncoding #{const SOX_ENCODING_DPCM})       = "encodingDPCM"
  show (SoxEncoding #{const SOX_ENCODING_DWVW})       = "encodingDWVW"
  show (SoxEncoding #{const SOX_ENCODING_DWVWN})      = "encodingDWVWN"
  show (SoxEncoding #{const SOX_ENCODING_GSM})        = "encodingGSM"
  show (SoxEncoding #{const SOX_ENCODING_MP3})        = "encodingMP3"
  show (SoxEncoding #{const SOX_ENCODING_VORBIS})     = "encodingVorbis"
  show (SoxEncoding #{const SOX_ENCODING_AMR_WB})     = "encodingAmrWB"
  show (SoxEncoding #{const SOX_ENCODING_AMR_NB})     = "encodingAmrNB"
  show (SoxEncoding #{const SOX_ENCODING_CVSD})       = "encodingCVSD"
  show (SoxEncoding #{const SOX_ENCODING_LPC10})      = "encodingLPC10"
  show (SoxEncoding #{const SOX_ENCODING_OPUS})       = "encodingOpus"
  show (SoxEncoding n) = error $ "libSoX: invalid encoding " ++ show n

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
