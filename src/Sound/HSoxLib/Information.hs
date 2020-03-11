module Sound.HSoxLib.Information
  ( AudioTrackGetter
  , runGetter
  , sigGetter
  , encGetter
  , commentsGetter

  -- * Getters
  -- $getters

  -- ** Signalinfo
  , sampleRateGetter
  , channelsGetter
  , precisionGetter
  , durationGetter
  -- ** Encodinginfo
  , encodingGetter
  , bitsPerSampleGetter
  -- ** Metadata
  , titleGetter
  , artistGetter
  , albumGetter
  , genreGetter
  , tracknumberGetter

  -- * Helpers

  , calcDuration
  , soxReadComments
  ) where

import           Control.Applicative  (liftA3)
import           Control.Monad.Reader (ReaderT (..))
import           Foreign.Ptr          (Ptr)
import           Foreign.Storable     (peek)
import           Text.Read            (readMaybe)

import qualified Sound.HSoxLib.FFI    as FFI
import qualified Sound.HSoxLib.Types  as T

-------------------------------------------------------------------------------

-- | Reader monad that used to read batch of audio file's information.
type AudioTrackGetter = ReaderT (Ptr T.SoxFormat) IO

runGetter :: AudioTrackGetter a -> Ptr T.SoxFormat -> IO a
runGetter = runReaderT

-- | To build signalinfo getter.
sigGetter :: (T.SoxSignalinfo -> a) -> AudioTrackGetter a
sigGetter rd = ReaderT $ \fmtptr ->
  let p = T.getFmtSignalPtr fmtptr
   in rd <$> peek p

-- | To build encodinginfo getter.
encGetter :: (T.SoxEncodinginfo -> a) -> AudioTrackGetter a
encGetter rd = ReaderT $ \fmtptr ->
  let p = T.getFmtEncodingPtr fmtptr
   in rd <$> peek p

-- | To build metadata getter.
commentsGetter :: (String -> Maybe a) -> String -> AudioTrackGetter (Maybe a)
commentsGetter rd key = ReaderT readComments
  where
   readComments fmtptr = do
     p <- T.getSoxCommentsPtr fmtptr
     soxReadComments p rd key

-------------------------------------------------------------------------------

{- $getters
A list of pre-defined getters, you can also define your own getters.

For example, you want to define a year getter to get the year from
file's metadata if there has.

> yearGetter :: AudioTrackGetter (Maybe Int)
> yearGetter = commentsGetter readMaybe "year"
-}

sampleRateGetter :: AudioTrackGetter (Maybe Double)
sampleRateGetter = sigGetter T.sigRate

channelsGetter :: AudioTrackGetter (Maybe Word)
channelsGetter = sigGetter T.sigChannels

precisionGetter :: AudioTrackGetter (Maybe Word)
precisionGetter = sigGetter T.sigPrecision

durationGetter :: AudioTrackGetter (Maybe Double)
durationGetter =
  let l = sigGetter T.sigLength
   in liftA3 (liftA3 calcDuration) l channelsGetter sampleRateGetter

encodingGetter :: AudioTrackGetter T.SoxEncoding
encodingGetter = encGetter T.encoding

bitsPerSampleGetter :: AudioTrackGetter Word
bitsPerSampleGetter = encGetter T.bitsPerSample

titleGetter :: AudioTrackGetter (Maybe String)
titleGetter = commentsGetter Just "title"

artistGetter :: AudioTrackGetter (Maybe String)
artistGetter = commentsGetter Just "artist"

albumGetter :: AudioTrackGetter (Maybe String)
albumGetter = commentsGetter Just "album"

genreGetter :: AudioTrackGetter (Maybe String)
genreGetter = commentsGetter Just "genre"

tracknumberGetter :: AudioTrackGetter (Maybe Int)
tracknumberGetter = commentsGetter readMaybe "tracknumber"

-------------------------------------------------------------------------------

-- | Calculate audio's duration in seconds.
calcDuration :: (Integral a1, Integral a2, Fractional b) => a1 -> a2 -> b -> b
calcDuration len channels rate = fromIntegral len / fromIntegral channels / rate

-- | Find file's metadata, pass it to an parser function.
soxReadComments :: Ptr T.SoxComments
                -> (String -> Maybe a)
                -- ^ Functin to parse the value that returned from
                -- 'FFI.soxFindComment'.
                -> String
                -- ^ Key passed to 'FFI.soxFindComment'.
                -> IO (Maybe a)
soxReadComments ptr rd key = do
  mval <- FFI.soxFindComment ptr key
  return $ mval >>= rd
