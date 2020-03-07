module Sound.HSoxLib.Types
  ( I.CFilePath
  , I.CFileType
  , I.SoxSample

  , I.SoxInt32
  , I.SoxInt64
  , I.SoxUInt32
  , I.SoxUInt64

  -- *
  , I.SoxBool
  , I.soxFalse
  , I.soxTrue

  -- *
  , I.SoxVersionInfo
  -- **
  , I.versionFlags
  , I.versionCode
  , I.version
  , I.versionExtra
  , I.buildTime
  , I.distro
  , I.compiler
  , I.arch

  -- *
  , I.SoxFormat
  -- **
  , I.fmtFilename
  , I.fmtSignalinfo
  , I.fmtEncodinginfo
  , I.fmtFiletype
  , I.fmtOOB
  , I.fmtSeekable
  , I.fmtMode
  , I.fmtOlength
  , I.fmtClips
  , I.fmtSoxErrno
  , I.fmtSoxErrStr
  , I.fmtIOType
  , I.fmtTellOff
  , I.fmtDataStart
  -- **
  , I.getFmtSignalPtr
  , I.getFmtEncodingPtr
  , I.getFmtOOBPtr

  -- *

  , I.SoxSignalinfo (..)
  , I.defaultSoxSignalinfo
  , I.freeSoxSignalinfoMult
  , I.freeSoxSignalinfoMult0

  , I.SoxEncodinginfo (..)

  , I.SoxOOB (..)
  , I.freeSoxOOBComments
  , I.freeSoxOOBComments0
  , I.freeSoxOOBInstr
  , I.freeSoxOOBInstr0
  , I.getSoxOOBCommentsPtr

  , I.SoxComments
  , getSoxCommentsPtr

  , I.SoxInstrinfo (..)

  , I.SoxLoopinfo (..)

  , I.SoxVersionFlag
  , I.soxFlagNone
  , I.soxFlagPopen
  , I.soxFlagMagic
  , I.soxFlagThreads
  , I.soxFlagMemopen

  , I.SoxError (..)
  , I.soxSuccess
  , I.soxEof
  , I.soxEhdr
  , I.soxEfmt
  , I.soxEnomem
  , I.soxEperm
  , I.soxEnotsup
  , I.soxEinval

  , I.SoxEncoding
  , I.encodingUnknown
  , I.encodingSign2
  , I.encodingUnsigned
  , I.encodingFloat
  , I.encodingFloatText
  , I.encodingFlac
  , I.encodingHcom
  , I.encodingWavpack
  , I.encodingWavpackf
  , I.encodingUlaw
  , I.encodingAlaw
  , I.encodingG721
  , I.encodingG723
  , I.encodingClADPCM
  , I.encodingClADPCM16
  , I.encodingMsADPCM
  , I.encodingImaADPCM
  , I.encodingOkiADPCM
  , I.encodingDPCM
  , I.encodingDWVW
  , I.encodingDWVWN
  , I.encodingGSM
  , I.encodingMP3
  , I.encodingVorbis
  , I.encodingAmrWB
  , I.encodingAmrNB
  , I.encodingCVSD
  , I.encodingLPC10
  , I.encodingOpus
  , I.soxEncodingsLen

  -- *

  , I.SoxEffectsChain
  , I.SoxEffect
  , I.SoxFlowEffectsCallback
  , I.SoxEffectHandler
  ) where

import           Foreign.Ptr          (Ptr)

import qualified Sound.HSoxLib.Types.Internal as I


-- | Get sox_comments ptr from sox_format.
getSoxCommentsPtr :: Ptr I.SoxFormat -> IO (Ptr I.SoxComments)
getSoxCommentsPtr fmtptr = I.getSoxOOBCommentsPtr $ I.getFmtOOBPtr fmtptr
