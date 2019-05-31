
{-|
  Module      : MC.Songbook.AllTheThingsYouAre
  Description : A tribute to the greatest Hip Hop producer of all time;
                Mr. J Dillalude.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.DillaFeel101 where

import MC.Midi.Compose

-- Try changing the arguments to "jDilla".
hh1, hh2, bd, sd :: AbstractPhrase
hh1 = rhythm $ qns 4
hh2 = rhythm $ dillaFeel (2 % 8) [1, 1] [4, 3] $ ens 8
bd  = rhythm $ measure [hn + en, hn - en]
sd  = (silence <> hit <> silence <> hit) :<: qns 4

dilla101 :: MidiComposition ()
dilla101 = do
  instrument (closedHihat   32); voice2midi $ play' (1 % 1) hh1
  instrument (closedHihat    1); voice2midi $ play' (4 % 5) hh2
  instrument (acousticSnare  9); voice2midi $ play' (3 % 4) sd
  instrument (electricSnare  8); voice2midi $ play' (1 % 1) sd
  instrument (bassDrum       1); voice2midi $ play' (1 % 1) bd
  where
    play' p = play . times 12 . velocity p

main :: IO ()
main =
  midiMC2File "./dilla.mid" 90 c_ lydian dilla101
