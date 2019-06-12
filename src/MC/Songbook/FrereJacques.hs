{-|
  Module      : MC.Songbook.FrereJacques
  Description : A French nursery rhyme.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.FrereJacques where

import MC.Midi.Compose

melody :: Voice ()
melody = do
  line' [i, ii, iii, i]        $ qns 4
  line' [iii, iv, v]           $ qns 2  <> hn
  line' [v, vi, v, iv, iii, i] $ den_sn <> ens 2 <> qns 2
  line' [i, v_, i]             $ qns 2  <> hn
  where line' notes rhythm = play . fortissimo $ times 2 $ motif notes rhythm

chords :: Voice ()
chords = play' $ triad1 i_ <> triad2 v__ <> triad1 i_ :<: qns 2 <> hns 1
  where play' = play . mezzoForte . times 8

main :: IO ()
main = midiMC2File "./frerejacques.mid" 105 g ionian $
  do melody `on` acousticGrandPiano
     chords `on` electricPiano1
