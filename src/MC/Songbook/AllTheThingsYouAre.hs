{-|
  Module      : MC.Songbook.AllTheThingsYouAre
  Description : An example of algorithmic comping.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.AllTheThingsYouAre where

import MC.Midi.Compose
import MC.Lib.Western

-- We wish to explore different strategies for comping for this standard.
type Strategy = [Chord] -> Voice AbstractPhrase

-- A simple strategy, can be to simply figure out the block of notes
-- that belong to each chord, and then spell them out.
blocks :: Strategy
blocks  cs    = return $ (foldr1 (<>) $ map chord1 $ twice cs) :<: r
  where r     = measure [hn, hn + qn, hn, qn, hn + qn, qn] <> r
        twice = foldr (\x xs -> x : x : xs) []

-- We could also pick some important notes, and play them one at the time.
walking :: Strategy
walking [      ] = return $ rest 0
walking [     c] = return $ note wn c
walking (c : cs) = do
  s   <- tonality
  aPh <- walking cs
  let (_ , Seq (Sound next : _), _) = unPhrase aPh
  let f a b = derive s (chord2mode a b)
  let (g, h) = case (f v c `compare` f i next, f i c `compare` f i next) of
        (EQ, _) -> ([i', v,   iii], flat )
        (_, EQ) -> ([i', v,   iii], sharp)
        (LT, _) -> ([i,  iii,   v], flat )
        _       -> ([i,  iii,   v], sharp)
  return $ (arpeggio (g ++ [next <> h]) c :<: qns 4) <> aPh

-- Most of the melody in All The Things You Are is just the thirds of the chords.
thirds :: Strategy
thirds cs = return $ (foldr1 (<>) $ map (\c -> arpeggio [iii] c) cs) :<: r
  where r = measure [wn, wn, wn + qn, hn + qn, wn + qn, hn + qn] <> r

-- Here are the chords for All The Things You Are (one chord per bar).
chords :: [Chord]
chords =
  [ f  <> min7 , bb <> min7, eb <> dom7, ab <> maj7
  , db <> maj7 , g  <> dom7, c  <> maj7, c  <> maj7
  , c  <> min7 , f  <> min7, bb <> dom7, eb <> maj7
  , ab <> maj7 , d  <> dom7, g  <> maj7, g  <> maj7
  , a  <> min7 , d  <> dom7, g  <> maj7, g  <> maj7
  , fs <> min7 , b  <> dom7, e  <> maj7, c  <> dom7 <> s5
  , f  <> min7 , bb <> min7, eb <> dom7, ab <> maj7
  , db <> maj7 , db <> min7, ab <> maj7, e  <> dom7 <> s9
  , bb <> min7 , eb <> dom7, ab <> maj7, ab <> maj7
  ]

-- Commonly, a piano plays the blocks, and a bass player plays the walking line:
main :: IO ()
main = do
  midiMC2File "./allTheThings.mid" 120 f aeolian $
    do pianobl `on` electricPiano1
       walk101 `on` acousticBass
       melody  `on` tenorSax
  where bound a b v ph = do
          s <- tonality
          let (a', b') = (derive s a, derive s b)
          play $ velocity v $ liftH (fmap $ fmap $ bounded a' b') ph
        effects = legato . modulate (1 % 16) . reverb (3 % 4)
        pianobl = blocks  chords >>= bound iv_  iv  (2 % 3)
        walk101 = walking chords >>= bound (fmap down i__) i_ 1 . octave (-2)
        melody  = thirds  chords >>= bound vi_ vi (5 % 6) . legato . modulate (1 % 8)
