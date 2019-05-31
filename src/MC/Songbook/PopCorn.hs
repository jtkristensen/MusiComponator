{-|
  Module      : MC.Midi.Compose
  Description : A tribute to the beginning of computer music.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.PopCorn where

import MC.Midi.Compose

-- Midway through the theme we change key to a relative major.
-- In this case a maj#11 chord, that we can draw from the lydian major mode.
relM11 :: AbstractPhrase -> Voice ()
relM11 aPh = tonality >>= \s -> play $ aPh `inKey` (lydian (derive s iii))

-- The bass line is built around an ostinato 'ost' mapped onto a list of chords.
-- Each chord is annotated with the duration at which it should be played.
bassLine :: Voice ()
bassLine = do
  play   $ bassRiff  $ intro
  play   $ times 2 $ bassRiff part1
  relM11 $ times 2 $ bassRiff part2
  play   $ times 2 $ bassRiff part1
  relM11 $ times 2 $ bassRiff part2
  where
    part1      = [(4, i), (4, i), (2, i),   (2, vii_), (2, vi_), (2, i)]
    part2      = [(4, i), (4, i), (2, iii), (2,  ii ), (4,  i)         ]
    intro      = map ((,)4) [i, i, i, i]
    bassRiff   = foldr1 (<>) . map (low . ost)
    low        = liftH $ fmap (mode $ relMode i_ . relMode i__)
    ost (1, c) =  arpeggio [i, iii, v, i'] c :<: sns 4
    ost (m, c) = (arpeggio [i, i', v] c :<: ens 1 <> sns 2) <> ost (m - 1, c)

-- The melody is split in two sections as well.
-- Moreover, the melody is split in parts that only differ on the rhythmic part.
melody :: Voice ()
melody = do
  play $ rest (4 - qn)
  part rhythm1 rhythm1' (arpeggio . ([iii, ii, iii     ]++)) iii
  part rhythm2 rhythm2' (arpeggio . ([iii, ii, iii, iii]++))   v
  where
    arppegios a1 a2 = line [i, ii] <> a1 i <> a1 vii_ <> a2 vi_
    rhythm1         = ens  6 <> qns 1
    rhythm1'        = ens 14 <> qns 1
    rhythm2         = ens 4 <> sns 1 <> ens 1 <> beat (5 % 16)
    rhythm2'        = ens  2 <> r2'' <> r2'' <> r2'' <> qns 1
    r2''            = ens 1 <> sns 1 <> ens 2 <> sns 1
    part r r' arp e =
      let a1 = line [  i, vii_,   i, v_, iii_, v_,   i_]     :<: r
          a2 = line [iii,  ii , iii,  i,   v_, i , iii_]     :<: r
          b1 = arppegios (arp [i]) (arp     [i]) <> line [i] :<: r'
          c1 = arppegios (arp [i]) (arp [iv, v]) <> line [e] :<: r'
          b2 = liftH (fmap (mode $ relMode iii)) b1
          c2 = liftH (fmap (mode $ relMode iii)) c1
      in do
        play   $ staccato ssn $ a1 <> a1 <> b1 <> a1 <> a1 <> c1
        relM11 $ staccato ssn $ a2 <> a2 <> b2 <> a2 <> a2 <> c2

-- Choosing a file-name, tempo and key.
-- Note that the recording is not in g minor, nor is it in Ab minor.
-- It is rather someting like (g + 1 % 3) minor {^_^}.
main :: IO ()
main = do
  midiMC2File "./popcorn.mid" 120 ((+(1 % 3)) <$> g) aeolian $
    do instrument synthBass1; voice2midi bassLine
       instrument reedOrgan;  voice2midi bassLine
       instrument synthBass2; voice2midi melody

