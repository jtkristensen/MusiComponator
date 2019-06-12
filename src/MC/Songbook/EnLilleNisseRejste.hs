{-|
  Module      : MC.Songbook.EnLilleNisseRejste
  Description : An exercise in transposition, based on a Danish children song
                from the collection "De små synger", transposed around all 12 keys.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.EnLilleNisseRejste where

import MC.Midi.Compose

-- The following exercise is dedicated my old piano teacher Olivier Antunes.
-- Changekey can be one turn in the circle of fifths, but you can come up with
-- alternative ways of changing keys (through all the modes or whatever).
transposed :: Voice () -> Int -> Voice () -> Voice ()
transposed changekey 0 v = return ()
transposed changekey n v = do
  s <- v >> changekey >> tonality
  transpose $ if root s < derive s g' then 12 else -12
  transposed changekey (n - 1) v

-- Here is the for a Danish children song about an elf
-- that traveled around the world.
melody :: Voice ()
melody = play $ fortissimo $ motif
  [ vi, v, iv, iii, ii, i, i, i
  , iv, iii, ii, i, vii_, vi_, v_, v_
  , i, i, i, vi, v, i, ii, iii, i, v, v, i] $
  foldr1 (<>)
  [ en -- upbeat.
  , ens 4, qn, ens 2, den_sn, ens 6, den_sn
  , ens 2, qn, ens 4, den_sn, dotted qn]

-- Here is a chord progression that follows the melody.
basicChords :: Chord -> Sequence Chord
basicChords = const $ foldr1 (<>) $ map triad1 $
    [  i,  v, i,  i, iv, iv, v,  v
    ,  i, iv, i, vi,  i,  v, i, i ]

-- Since the above chord progression some times repeats the same chord
-- over two quaters, the song kind of looses momentum, and we may
-- decide to fill in the gaps with bi-dominant chords.
-- This will also help the tune sound like it is constantly
-- "traveling aroung" like the elf in the lyric.
ellaborateChords :: Chord -> Sequence Chord
ellaborateChords next = foldr1 (<>) $ map chord1 $
    [  i,  v, i,  bd iv, iv, bd  v,  v, bd i
    ,  i, iv, i,  bd iv, iv,  v, i, bd next]
  where bd chord = chord <> v <> dom7

-- After transposing aroung the chords systematically, some of them may
-- end up at a high or low frequency. So, we map the chords into
-- the octave around the middle c.
chords :: Sequence Chord -> Voice ()
chords cs = between c c' (cs :<: qns 16) >>= play . mezzoForte . legato

-- We output the melody in all major keys around the circle of fifths,
-- starting at c ionian in both directions. The instrumentation is mainly
-- to get out the christmas mood {^_^}.
main :: IO ()
main = do
  midiMC2File "./transExeL.mid"  90 c ionian $
    do mv c5outerl melody           `on` drawbarOrgan
       mv c5outerl melody           `on` celesta
       cv c5outerl (chords $ cs iv) `on` electricPiano1
       play (piano upbeats)         `on` rideBell 1
  midiMC2File "./transExeR.mid"  90 c ionian $
    do mv c5outerr melody           `on` drawbarOrgan
       mv c5outerr melody           `on` celesta
       cv c5outerr (chords $ cs  v) `on` electricPiano1
       play (piano upbeats)         `on` rideBell 1
  where cs = ellaborateChords -- Decide on a harmonization (ellaborate/basic).
        cv changekey v = play (rest $ duration en) <> transposed changekey 13 v
        mv changekey v =                              transposed changekey 13 v
        upbeats        = (rest $ duration qn) <> (rhythm $ qns $ 16 * 13)
