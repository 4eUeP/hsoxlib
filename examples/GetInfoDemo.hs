module Main (main) where

import           Data.Maybe          (fromMaybe)
import           System.Environment  (getArgs)

import qualified Sound.HSoxLib       as Sox
import qualified Sound.HSoxLib.Types as T

data AudioTrack =
  AudioTrack { artist         :: Maybe String
             , album          :: Maybe String
             , sampleRate     :: Maybe Double
             , sampleEncoding :: T.SoxEncoding
             } deriving (Show)

audioTrackGetter :: Sox.AudioTrackGetter AudioTrack
audioTrackGetter =
  AudioTrack <$> Sox.artistGetter
             <*> Sox.albumGetter
             <*> Sox.sampleRateGetter
             <*> Sox.encodingGetter

getAudioTrack :: FilePath -> IO AudioTrack
getAudioTrack filepath =
  Sox.simpleSoxOpenRead filepath $ Sox.runGetter audioTrackGetter

main :: IO ()
main = Sox.withSox $ do
  filepath <- head <$> getArgs
  audioTrack <- getAudioTrack filepath
  print audioTrack
