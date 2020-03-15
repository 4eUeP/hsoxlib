HSoxLib
=======

Haskell bindings to [LibSoX](http://sox.sourceforge.net/),
the Swiss Army knife of sound processing programs.

Tested on debian bullseye with libsox 14.4.2 and gcc 9.2.1 installed via `apt`.

**Warning: this library is unofficially and still in development.**


## Quick start

Make sure you already have the c library installed, for debian user, you can
simply install it by:

```
apt-get install libsox-dev
```

Check version number of libsox you have:

(the version should no less than `14.4.0`)

```haskell
>>> import Sound.HSoxLib
>>> soxVersion
"14.4.2"
```

#### Getting audio file's information

```haskell
import qualified Sound.HSoxLib       as Sox
import qualified Sound.HSoxLib.Types as T

data AudioTrack =
  AudioTrack { artist         :: Maybe String
             , album          :: Maybe String
             , sampleRate     :: Maybe Double
             , sampleEncoding :: T.SoxEncoding
             } deriving Show

audioTrackGetter :: Sox.AudioTrackGetter AudioTrack
audioTrackGetter =
  AudioTrack <$> Sox.artistGetter
             <*> Sox.albumGetter
             <*> Sox.sampleRateGetter
             <*> Sox.encodingGetter

getAudioTrack :: IO AudioTrack
getAudioTrack =
  let filepath = "/home/un/Music/test.flac"
   in Sox.simpleSoxOpenRead filepath $ Sox.runGetter audioTrackGetter

main :: IO ()
main = Sox.withSox $ do
  audioTrack <- getAudioTrack
  print audioTrack
```

More examples are in `examples` directory.


## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
