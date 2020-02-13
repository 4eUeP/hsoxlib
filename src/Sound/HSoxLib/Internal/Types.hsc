{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-identities         #-}

-- | Do not use this module, see 'Sound.HSoxLib.Types' instead
module Sound.HSoxLib.Internal.Types where

#include <sox.h>

import           Data.Bits                    ((.&.))
import           Data.Int
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

-- | Signal parameters.
data SoxSignalinfo =
  SoxSignalinfo { rate      :: Maybe Double
                -- ^ samples per second
                , channels  :: Maybe Word
                -- ^ number of sound channels
                , precision :: Maybe Word
                -- ^ bits per sample
                , length    :: Maybe Word64
                -- ^ samples * chans in file, @-1@ if unspecified.
                , mult      :: Maybe Double
                -- ^ effects headroom multiplier
                } deriving (Show)

instance Storable SoxSignalinfo where
  sizeOf _ = #{size sox_signalinfo_t}
  alignment _ = #{alignment sox_signalinfo_t}
  peek ptr =
    pure SoxSignalinfo
      <*> (fmap maybeUnspec $ #{peek_double sox_signalinfo_t, rate} ptr)
      <*> (fmap maybeUnspec $ #{peek_int sox_signalinfo_t, channels} ptr)
      <*> (fmap maybeUnspec $ #{peek_int sox_signalinfo_t, precision} ptr)
      <*> (fmap maybeUnspec $ #{peek_int sox_signalinfo_t, length} ptr)
      <*> (U.peekCDoubleNull =<< #{peek sox_signalinfo_t, mult} ptr)
    where
      maybeUnspec :: (Num a, Eq a) => a -> Maybe a
      maybeUnspec n
        | n == #{const SOX_UNSPEC} = Nothing
        | otherwise = Just n
  poke = error "SoxSignalinfo.poke: NotImplemented."

-- | Encoding parameters.
data SoxEncodinginfo =
  SoxEncodinginfo { encoding       :: SoxEncoding
                  -- ^ format of sample numbers
                  , bitsPerSample  :: Word
                  -- ^ 0 if unknown or variable; uncompressed value if lossless;
                  -- compressed value if lossy
                  , compression    :: Double
                  -- ^ compression factor (where applicable)
                  , reverseBytes   :: SoxOption
                  -- ^ Should bytes be reversed? If this is default during
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_read' or
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_write',
                  -- libSoX will set them to either no or yes according to
                  -- the machine or format default.
                  , reverseNibbles :: SoxOption
                  -- ^ Should nibbles be reversed? If this is default during
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_read' or
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_write',
                  -- libSoX will set them to either no or yes according to
                  -- the machine or format default.
                  , reverseBits    :: SoxOption
                  -- ^ Should bits be reversed? If this is default during
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_read' or
                  -- 'Sound.HSoxLib.Internal.FFI.c_sox_open_write',
                  -- libSoX will set them to either no or yes according to
                  -- the machine or format default.
                  , oppositeEndian :: SoxBool
                  -- ^ If set to true, the format should reverse its default
                  -- endianness.
                  } deriving (Show, Eq)

instance Storable SoxEncodinginfo where
  sizeOf _ = #{size sox_encodinginfo_t}
  alignment _ = #{alignment sox_encodinginfo_t}
  peek ptr =
    pure SoxEncodinginfo
      <*> (#{peek sox_encodinginfo_t, encoding} ptr)
      <*> (#{peek_int sox_encodinginfo_t, bits_per_sample} ptr)
      <*> (#{peek_double sox_encodinginfo_t, compression} ptr)
      <*> (#{peek sox_encodinginfo_t, reverse_bytes} ptr)
      <*> (#{peek sox_encodinginfo_t, reverse_nibbles} ptr)
      <*> (#{peek sox_encodinginfo_t, reverse_bits} ptr)
      <*> (#{peek sox_encodinginfo_t, opposite_endian} ptr)
  poke = error "SoxEncodinginfo.poke: NotImplemented."


-- | The libsox-specific error codes.
newtype SoxError = SoxError { soxECode :: C.CInt }
  deriving (Eq, Storable)

#{enum SoxError, SoxError
  , soxSuccess = SOX_SUCCESS
  , soxEof     = SOX_EOF
  , soxEhdr    = SOX_EHDR
  , soxEfmt    = SOX_EFMT
  , soxEnomem  = SOX_ENOMEM
  , soxEperm   = SOX_EPERM
  , soxEnotsup = SOX_ENOTSUP
  , soxEinval  = SOX_EINVAL
 }

instance Show SoxError where
  show (SoxError (#{const SOX_SUCCESS})) = "Succeeded"
  show (SoxError (#{const SOX_EOF}))     = "End Of File or other error"
  show (SoxError #{const SOX_EHDR})      = "Invalid Audio Header"
  show (SoxError #{const SOX_EFMT})      = "Unsupported data format"
  show (SoxError #{const SOX_ENOMEM})    = "Can't alloc memory"
  show (SoxError #{const SOX_EPERM})     = "Operation not permitted"
  show (SoxError #{const SOX_ENOTSUP})   = "Operation not supported"
  show (SoxError #{const SOX_EINVAL})    = "Invalid argument"
  show (SoxError n) = "libSoX: unknown error code " ++ show n

-- | libsox boolean type
newtype SoxBool = SoxBool #{type sox_bool}
  deriving (Eq, Storable)

#{enum SoxBool, SoxBool
  , soxFalse = sox_false
  , soxTrue  = sox_true
 }

instance Show SoxBool where
  show (SoxBool #{const sox_false}) = "false"
  show (SoxBool #{const sox_true})  = "true"
  show (SoxBool n) = error $ "libSoX: invalid SoxBool " ++ show n

-- | no, yes, or default
-- (default usually implies some kind of auto-detect logic).
newtype SoxOption = SoxOption #{type sox_option_t}
  deriving (Eq, Storable)

#{enum SoxOption, SoxOption
  , soxOptionNo      = sox_option_no
  , soxOptionYes     = sox_option_yes
  , soxOptionDefault = sox_option_default
 }

instance Show SoxOption where
  show (SoxOption #{const sox_option_no})      = "option_no"
  show (SoxOption #{const sox_option_yes})     = "option_yes"
  show (SoxOption #{const sox_option_default}) = "option_default"
  show (SoxOption n) = error $ "libSoX: invalid SoxOption " ++ show n

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

-- | Is file a real file, a pipe, or a url?
newtype LsxIOType = LsxIOType #{type lsx_io_type}
  deriving (Eq, Storable)

#{enum LsxIOType, LsxIOType
  , ioFile = lsx_io_file
  , ioPipe = lsx_io_pipe
  , ioURL  = lsx_io_url
 }

instance Show LsxIOType where
  show (LsxIOType #{const lsx_io_file}) = "file"
  show (LsxIOType #{const lsx_io_pipe}) = "pipe"
  show (LsxIOType #{const lsx_io_url})  = "url"
  show (LsxIOType n) = error $ "libSoX: invalid LsxIOType " ++ show n

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
