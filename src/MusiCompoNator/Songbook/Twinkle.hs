
module MusiCompoNator.Songbook.Twinkle where

import MusiCompoNator.Composition
import MusiCompoNator.Core
import MusiCompoNator.GeneralMidi
import ZMidi.Core
import Data.Ratio

-- Melody as is.
melody :: Voice ()
melody = singleV $ (a <> b <> c <> c <> a <> b) :<: r
  where a = line [i,   i,    v,   v,  vi,  vi,  v] -- Twinkle twinkle ..
        b = line [iv, iv,  iii, iii,  ii,  ii,  i] -- How I wonder ..
        c = line [ v,  v,   iv,  iv, iii, iii, ii] -- Up above the ..
        r = qns 6 <> hns 1 <> r

-- Chords on off-beats.
chords :: Voice ()
chords = singleV $ rest (1 % 8) <> ((a <> b <> c <> c <> a <> b) :<: r)
  where one  = chord 1 [v,    iii_, i   ]
        four = chord 2 [vii_, iii , i_  ]
        five = chord 5 [i_,   iii , vii_]
        a    = one  <> one  <> four <> one
        b    = four <> one  <> five <> one
        c    = one  <> four <> one  <> five
        r    = hns 1 <> r

-- Bass line, derived from the chord scheme.
bassline :: Voice ()
bassline = singleV $ foldr1 (<>) bass :<: r
  where pattern = [i] : [v, mis, iii] : [i] : [v, i, iii] : pattern
        bass    = zipWith (\p f -> f p) (map (map ((.) $ down . down)) pattern) $
          a <> b <> c <> c <> a <> b
        a = map arpeggio [1, 1, 4, 1]
        b = map arpeggio [4, 1, 5, 1]
        c = map arpeggio [1, 4, 1, 5]
        r = dotted (qns 2) <> ens 2  <> r

-- Sometimes, mucisians need to rest.
take12 :: Voice ()
take12 = singleV $ rest 12

-- Put the peices together.
composition :: MidiComposition ()
composition = do
  harmonica    $ take12   <> melody   <> melody
  marimba      $ chords   <> take12   <> chords
  acousticBass $ bassline <> bassline <> bassline
  closeMC

-- Served as a file.
main :: IO ()
main = do
  writeMidi "./twinklemajor.mid" $ fst $ runMidiComposition (ionian  c) composition
  writeMidi "./twinkleminor.mid" $ fst $ runMidiComposition (aeolian c) composition
