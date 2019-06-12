{-|
  Module      : MC.Songbook.Twinkle
  Description : A "hello world" for music programming languages.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.Twinkle where

import MC.Midi.Compose

melody :: Voice AbstractPhrase
melody = return $ a <> b <> c <> c <> a <> b :<: r
  where
    a = line [ i,  i,    v,   v,  vi,  vi,  v] -- Twinkle twinkle ..
    b = line [iv, iv,  iii, iii,  ii,  ii,  i] -- How I wonder    ..
    c = line [ v,  v,   iv,  iv, iii, iii, ii] -- Up above the    ..
    r = qns 6 <> hn <> r

chords :: Voice AbstractPhrase
chords = return $ a <> b <> c <> c <> a <> b :<: r
  where
    scheme = foldr1 (<>) . map triad1
    a = scheme [i, i, iv, i]
    b = scheme [iv, i, v, i]
    c = scheme [i, iv, i, v]
    r = hn <> r

twinkle :: MidiComposition ()
twinkle = do
  instrument harmonica
  voice2midi $ melody >>= play
  instrument acousticGrandPiano
  voice2midi $ fmap (velocity (3 % 4)) chords >>= between iv_ iv >>= play

main :: IO ()
main = do
  midiMC2File "./twinklemajor.mid" 120 c ionian  twinkle
  midiMC2File "./twinkleminor.mid" 120 c aeolian twinkle
