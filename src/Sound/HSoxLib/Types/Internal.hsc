{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wno-identities         #-}

-- | Do not use this module, see 'Sound.HSoxLib.Types' instead
module Sound.HSoxLib.Types.Internal where

#include <sox.h>

import           Data.Bits           ((.&.))
import           Data.Int
import           Data.Maybe          (fromMaybe)
import           Data.Word
import qualified Foreign.C.String    as C
import qualified Foreign.C.Types     as C
import qualified Foreign.Marshal     as M
import           Foreign.Ptr
import           Foreign.Storable

import qualified Sound.HSoxLib.Utils as U

-------------------------------------------------------------------------------

type FileType = String

type CFileType = C.CString

type CFilePath = C.CString

type SoxSample = SoxInt32
type SoxComments = [String]

type SoxInt32 = #{type sox_int32_t}
type SoxInt64 = #{type sox_int64_t}
type SoxUInt32 = #{type sox_uint32_t}
type SoxUInt64 = #{type sox_uint64_t}

-------------------------------------------------------------------------------

-- | Information about a build of libsox.
--
-- Some fields (for example, 'versionExtra', 'distro', 'compiler') will
-- return an empty string if there is nothing about this information.
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

-- | Data passed to/from the format handler.
data SoxFormat =
  SoxFormat { fmtFilename     :: FilePath
            -- ^ File name
            , fmtSignalinfo   :: SoxSignalinfo
            -- ^ Signal specifications for reader (decoder) or
            -- writer (encoder).
            , fmtEncodinginfo :: SoxEncodinginfo
            -- ^ Encoding specifications for reader (decoder) or
            -- writer (encoder).
            , fmtFiletype     :: FileType
            -- ^ Type of file, as determined by header inspection or libmagic.
            , fmtOOB          :: SoxOOB
            -- ^ Out-of-band data.
            , fmtSeekable     :: SoxBool
            -- ^ Can seek on this file ?
            , fmtMode         :: Char
            -- ^ Read or write mode ('r' or 'w')
            , fmtOlength      :: Word64
            -- ^ samples * chans written to file
            , fmtClips        :: Word64
            -- ^ Incremented if clipping occurs
            , fmtSoxErrno     :: C.CInt
            -- ^ Failure error code
            , fmtSoxErrStr    :: String
            -- ^ Failure error text
            , fmtFp           :: Ptr C.CFile
            -- ^ File stream pointer
            , fmtIOType       :: LsxIOType
            -- ^ Stores whether this is a file, pipe or URL.
            , fmtTellOff      :: Word64
            -- ^ Current offset within file.
            , fmtDataStart    :: Word64
            -- ^ Offset at which headers end and sound data begins.
            , fmtHandler      :: Ptr SoxFormatHandler
            -- ^ Format handler for this file
            , fmtPriv         :: Ptr ()
            -- ^ Format handler's private data area
            }

instance Storable SoxFormat where
  sizeOf _ = #{size sox_format_t}
  alignment _ = #{alignment sox_format_t}
  peek ptr =
    pure SoxFormat
      <*> (U.peekCStringEmpty =<< #{peek sox_format_t, filename} ptr)
      <*> #{peek sox_format_t, signal} ptr
      <*> #{peek sox_format_t, encoding} ptr
      <*> (U.peekCStringEmpty =<< #{peek sox_format_t, filetype} ptr)
      <*> #{peek sox_format_t, oob} ptr
      <*> #{peek sox_format_t, seekable} ptr
      <*> #{peek sox_format_t, mode} ptr
      <*> #{peek_int sox_format_t, olength} ptr
      <*> #{peek_int sox_format_t, clips} ptr
      <*> #{peek sox_format_t, sox_errno} ptr
      -- Caution: sox_errstr in libsox is 256 length char array,
      -- however, here is not.
      <*> (C.peekCString $ #{ptr sox_format_t, sox_errstr} ptr)
      <*> #{peek sox_format_t, fp} ptr
      <*> #{peek sox_format_t, io_type} ptr
      <*> #{peek_int sox_format_t, tell_off} ptr
      <*> #{peek_int sox_format_t, data_start} ptr
      <*> #{peek sox_format_t, handler} ptr
      <*> #{peek sox_format_t, priv} ptr
  poke = error "SoxFormat.poke: NotImplemented."

getFmtSignalPtr :: Ptr SoxFormat -> Ptr SoxSignalinfo
getFmtSignalPtr = #{ptr sox_format_t, signal}

getFmtEncodingPtr :: Ptr SoxFormat -> Ptr SoxEncodinginfo
getFmtEncodingPtr = #{ptr sox_format_t, encoding}

-------------------------------------------------------------------------------

-- | Signal parameters.
data SoxSignalinfo =
  SoxSignalinfo { sigRate      :: Maybe Double
                -- ^ samples per second
                , sigChannels  :: Maybe Word
                -- ^ number of sound channels
                , sigPrecision :: Maybe Word
                -- ^ bits per sample
                , sigLength    :: Maybe Word64
                -- ^ samples * chans in file, @-1@ if unspecified.
                , sigMult      :: Maybe Double
                -- ^ effects headroom multiplier
                } deriving (Show, Eq)

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
  poke ptr SoxSignalinfo{..} = rate' >> chan' >> prec' >> leng' >> mult'
    where
      rate' = #{poke_double sox_signalinfo_t, rate} ptr $ set sigRate
      chan' = #{poke sox_signalinfo_t, channels } ptr $ set sigChannels
      prec' = #{poke sox_signalinfo_t, precision} ptr $ set sigPrecision
      leng' = #{poke sox_signalinfo_t, length} ptr $ set sigLength
      mult' = let x = C.CDouble <$> sigMult
               in #{poke sox_signalinfo_t, mult} ptr =<< (U.fromMaybeNew x)
      set :: Num a => Maybe a -> a
      set = fromMaybe (#{const SOX_UNSPEC})

freeSoxSignalinfoMult :: Ptr SoxSignalinfo -> IO ()
freeSoxSignalinfoMult ptr = M.free =<< #{peek sox_signalinfo_t, mult} ptr

freeSoxSignalinfoMult0 :: Ptr SoxSignalinfo -> IO ()
freeSoxSignalinfoMult0 ptr = freeSoxSignalinfoMult ptr >> setNull ptr
  where
    setNull p = #{poke sox_signalinfo_t, mult} p nullPtr

defaultSoxSignalinfo :: SoxSignalinfo
defaultSoxSignalinfo = SoxSignalinfo Nothing Nothing Nothing Nothing Nothing

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

-- | Basic information about an encoding.
data SoxEncodingsInfo =
  SoxEncodingsInfo { encodingFlags :: SoxEncodingsFlag
                   , encodingName  :: String
                   , encodingDesc  :: String
                   } deriving (Show, Eq)

instance Storable SoxEncodingsInfo where
  sizeOf _ = #{size sox_oob_t}
  alignment _ = #{alignment sox_oob_t}
  peek ptr =
    pure SoxEncodingsInfo
      <*> #{peek sox_encodings_info_t, flags} ptr
      <*> (U.peekCStringEmpty =<< #{peek sox_encodings_info_t, name} ptr)
      <*> (U.peekCStringEmpty =<< #{peek sox_encodings_info_t, desc} ptr)
  poke = error "SoxEncodingsInfo.poke: NotImplemented."

-- | Comments, instrument info, loop info (out-of-band data).
data SoxOOB =
  SoxOOB { oobComments :: SoxComments
         -- ^ File's metadata in key=value format.
         , oobInstr    :: Maybe SoxInstrinfo
         -- ^ Instrument specification.
         , oobLoops    :: [SoxLoopinfo]
         -- ^ Looping specification.
         -- Caution: in libsox, sox_oob_t.loops is a fixed length array.
         } deriving (Show, Eq)

instance Storable SoxOOB where
  sizeOf _ = #{size sox_oob_t}
  alignment _ = #{alignment sox_oob_t}
  peek ptr =
    pure SoxOOB
      <*> (U.peekArrayCStrings =<< #{peek sox_oob_t, comments} ptr)
      <*> (U.peekMaybeNull =<< #{peek sox_oob_t, instr} ptr)
      <*> let p = #{ptr sox_oob_t, loops} ptr
              pSize = sizeOf p
              loopSize = sizeOf (undefined :: SoxLoopinfo)
              toArrayLen n =
                let (d, m) = (n * loopSize) `divMod` pSize
                    errmsg = "You should never see this error, details: "
                          <> "n: " ++ show n
                          <> ", loopSize: " ++ show loopSize
                          <> ", pSize: " ++ show pSize
                 in if m == 0 then d else error errmsg
              toLoopsLen l = let (d, m) = (l * pSize) `divMod` loopSize
                              -- there is no reason this "else" will happen,
                              -- however, if it really happened, we choose
                              -- 'soxMaxNloops'.
                              in if m == 0 then d else soxMaxNloops
              maxArrayLen = toArrayLen soxMaxNloops
           in do arrayLen <- U.lengthArray0WithMax maxArrayLen nullPtr p
                 M.peekArray (toLoopsLen arrayLen) p
  poke ptr SoxOOB{..} = comm' >> inst' >> loop'
    where
      comm' = do
        p' <- U.makeCStringArray0 nullPtr oobComments
        #{poke sox_oob_t, comments} ptr p'
      inst' =
        case oobInstr of
          Nothing -> #{poke sox_oob_t, instr} ptr nullPtr
          Just v  -> do
            p' <- M.malloc
            poke p' v
            #{poke sox_oob_t, instr} ptr p'
      loop'
        | length oobLoops > soxMaxNloops =
            error $ "SoxOOB.poke: The max size of loops is "
                 <> show soxMaxNloops ++ "."
        | otherwise = do
            p' <- M.callocArray soxMaxNloops
            M.pokeArray p' oobLoops
            M.copyArray (#{ptr sox_oob_t, loops} ptr) p' soxMaxNloops

soxMaxNloops :: Int
soxMaxNloops = #{const SOX_MAX_NLOOPS}

freeSoxOOBComments :: Ptr SoxOOB -> IO ()
freeSoxOOBComments ptr = M.free =<< #{peek sox_oob_t, comments} ptr

freeSoxOOBComments0 :: Ptr SoxOOB -> IO ()
freeSoxOOBComments0 ptr = freeSoxOOBComments ptr >> setNull ptr
  where
    setNull p = #{poke sox_oob_t, comments} p nullPtr

freeSoxOOBInstr :: Ptr SoxOOB -> IO ()
freeSoxOOBInstr ptr = M.free =<< #{peek sox_oob_t, instr} ptr

freeSoxOOBInstr0 :: Ptr SoxOOB -> IO ()
freeSoxOOBInstr0 ptr = freeSoxOOBInstr ptr >> setNull ptr
  where
    setNull p = #{poke sox_oob_t, instr} p nullPtr

-- | Instrument information.
data SoxInstrinfo =
  SoxInstrinfo { midiNote :: Int8
               -- ^ for unity pitch playback
               , midiLow  :: Int8
               -- ^ MIDI pitch-bend low range
               , midiHi   :: Int8
               -- ^ MIDI pitch-bend high range
               , loopMode :: Word8
               -- ^ 0=no, 1=forward, 2=forward/back, ...
               -- TODO: should this be 'SoxLoopFlag'?
               , nloops   :: Word
               -- ^ number of active loops (max SOX_MAX_NLOOPS)
               } deriving (Show, Eq)

instance Storable SoxInstrinfo where
  sizeOf _ = #{size sox_instrinfo_t}
  alignment _ = #{alignment sox_instrinfo_t}
  peek ptr =
    pure SoxInstrinfo
      <*> #{peek sox_instrinfo_t, MIDInote} ptr
      <*> #{peek sox_instrinfo_t, MIDIlow} ptr
      <*> #{peek sox_instrinfo_t, MIDIhi} ptr
      <*> #{peek sox_instrinfo_t, loopmode} ptr
      <*> #{peek sox_instrinfo_t, nloops} ptr
  poke ptr SoxInstrinfo{..} = note' >> mlow' >> mhig' >> mode' >> nlps'
    where
      note' = #{poke sox_instrinfo_t, MIDInote} ptr midiNote
      mlow' = #{poke sox_instrinfo_t, MIDIlow} ptr midiLow
      mhig' = #{poke sox_instrinfo_t, MIDIhi} ptr midiHi
      mode' = #{poke sox_instrinfo_t, loopmode} ptr loopMode
      nlps' = #{poke sox_instrinfo_t, nloops} ptr nloops

-- | Looping parameters (out-of-band data).
data SoxLoopinfo =
  SoxLoopinfo { loopStart  :: Word64
              -- ^ first sample
              , loopLength :: Word64
              -- ^ length
              , loopCount  :: Word
              -- ^ number of repeats, 0=forever
              , loopType   :: Word8
              -- ^ 0=no, 1=forward, 2=forward/back, ...
              -- TODO: should this be 'SoxLoopFlag'?
              } deriving (Show, Eq)

instance Storable SoxLoopinfo where
  sizeOf _ = #{size sox_loopinfo_t}
  alignment _ = #{alignment sox_loopinfo_t}
  peek ptr =
    pure SoxLoopinfo
      <*> (#{peek sox_loopinfo_t, start} ptr)
      <*> (#{peek sox_loopinfo_t, length} ptr)
      <*> (#{peek sox_loopinfo_t, count} ptr)
      <*> (#{peek sox_loopinfo_t, type} ptr)
  poke ptr SoxLoopinfo{..} = strt' >> leng' >> coun' >> type'
    where
      strt' = #{poke sox_loopinfo_t, start} ptr loopStart
      leng' = #{poke sox_loopinfo_t, length} ptr loopLength
      coun' = #{poke sox_loopinfo_t, count} ptr loopCount
      type' = #{poke sox_loopinfo_t, type} ptr loopType

-------------------------------------------------------------------------------

-- | The libsox-specific error codes.
--
-- The implementation is deliberately exposed. :)
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

-- | Loop modes: upper 4 bits mask the loop blass, lower 4 bits describe
-- the loop behaviour, for example single shot, bidirectional etc.
newtype SoxLoopFlag = SoxLoopFlag Word
  deriving (Eq, Storable)

#{enum SoxLoopFlag, SoxLoopFlag
  , loopNone         = sox_loop_none
  , loopForward      = sox_loop_forward
  , loopForwardBack  = sox_loop_forward_back
  , loop8            = sox_loop_8
  , loopSustainDecay = sox_loop_sustain_decay
 }

instance Show SoxLoopFlag where
  show (SoxLoopFlag #{const sox_loop_none})          = "loop_none"
  show (SoxLoopFlag #{const sox_loop_forward})       = "loop_forward"
  show (SoxLoopFlag #{const sox_loop_forward_back})  = "loop_forward_back"
  show (SoxLoopFlag #{const sox_loop_8})             = "loop_8"
  show (SoxLoopFlag #{const sox_loop_sustain_decay}) = "loop_sustain_decay"
  show (SoxLoopFlag n) = error $ "libSoX: invalid SoxLoopFlag " ++ show n

-- | Flags for 'SoxEncodingsInfo': lossless/lossy1/lossy2.
--
-- lossy once (lossy1), lossy twice (lossy2), or lossless (none)
newtype SoxEncodingsFlag = SoxEncodingsFlag #{type sox_encodings_flags_t}
  deriving (Eq, Storable)

#{enum SoxEncodingsFlag, SoxEncodingsFlag
  , soxEncodingsNone   = sox_encodings_none
  , soxEncodingsLossy1 = sox_encodings_lossy1
  , soxEncodingsLossy2 = sox_encodings_lossy2
 }

instance Show SoxEncodingsFlag where
  show (SoxEncodingsFlag #{const sox_encodings_none})   = "none"
  show (SoxEncodingsFlag #{const sox_encodings_lossy1}) = "lossy1"
  show (SoxEncodingsFlag #{const sox_encodings_lossy2}) = "lossy2"
  show (SoxEncodingsFlag n) =
    error $ "libSoX: invalid SoxEncodingsFlag " ++ show n

-- | Type of plot.
newtype SoxPlot = SoxPlot #{type sox_plot_t}
  deriving (Eq, Storable)

#{enum SoxPlot, SoxPlot
  , soxPlotOff     = sox_plot_off
  , soxPlotOctave  = sox_plot_octave
  , soxPlotGnuplot = sox_plot_gnuplot
  , soxPlotData    = sox_plot_data
 }

instance Show SoxPlot where
  show (SoxPlot #{const sox_plot_off})     = "off"
  show (SoxPlot #{const sox_plot_octave})  = "octave"
  show (SoxPlot #{const sox_plot_gnuplot}) = "gnuplot"
  show (SoxPlot #{const sox_plot_data})    = "data"
  show (SoxPlot n) = error $ "libSoX: invalid SoxPlot " ++ show n

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

data SoxEffectsChain

data SoxEffect

-------------------------------------------------------------------------------

data SoxFormatHandler

data SoxEffectHandler

-------------------------------------------------------------------------------

type SoxFlowEffectsCallback a = SoxBool -> Ptr a -> C.CInt

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
