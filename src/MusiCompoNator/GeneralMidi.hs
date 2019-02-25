
module MusiCompoNator.GeneralMidi where

import MusiCompoNator.Core
import MusiCompoNator.Composition
import ZMidi.Core
import Data.Ratio
import Data.Word

-- Input  : key-pressed, pitchbend-sensitivity, target ratio
-- Outpus : 14-bit pitch-bend value (as interpreted in ZMidi.Core).
pbValue :: Word8 -> Word8 -> Pitch -> Word14
pbValue key sense value = fromIntegral $ n `div` d + 8192
  where k = (value - (fromIntegral key)) * (8191 % fromIntegral sense)
        n = numerator   k
        d = denominator k

-- For now, we assume sense = 1, and bend upwards.
-- Later, we can have a 'pitch bend' operator {^_^} ?
bend :: Pitch -> Word14
bend p = pbValue (keyPress p) 1 p

-- Which 'piano key' to press.
keyPress :: Rational -> Word8
keyPress q = fromIntegral $ (numerator q `div` denominator q)

-- Find the minimum common divisor of all beats.
beatSize :: Motif a -> Beat -> Integer
beatSize m q = foldl lcm (denominator q) (map denominator $ map fst $ listEvents m)

-- Compute the number of ticks to represent 't' using 'beatSize' beats.
ticks :: Integer -> Beat -> Integer
ticks d t = (d `div` denominator t) * numerator t

-- Bpm is implicitly (1 % 4) = bpm, so, we can compute tpm directly.
trackHead :: Int -> String -> [MidiMessage]
trackHead bpm title = [ (0, MetaEvent $ TextEvent SEQUENCE_NAME title)
                      , (0, MetaEvent $ SetTempo tpm) ]
  where
    tpm = (floor $ (10^6*60) / fromIntegral bpm)

-- All midi-files ends like this.
trackFoot :: [MidiMessage]
trackFoot = [(0, MetaEvent EndOfTrack)]


type MidiComposition = Composition () [(Beat, MidiEvent)]

-- data Motif  a = Motif [a] Rhythm | (Motif a) :+: (Motif a)
-- type Phrase a = Scale -> Motif (Simultanity Pitch, [a])
-- data Simultanity a =
--     Silence
--   | Sound a
--   | (Simultanity a) :=: (Simultanity a)

listEvents :: Motif a -> [(Beat, a)]
listEvents (Motif h r) = zip r h
listEvents (m0 :+: m1) = listEvents m0 ++ listEvents m1

chord1 :: Phrase ()
chord1 = play (arpeggio 1 [i, iii <~ flat, v] `with` repeat en)

chord2 :: Phrase ()
chord2 = play ([voicing 2 [i, iii <~ flat, v, up . i]] `with` repeat en)

melody :: Voice ()
melody = Voice (ionian c) $ chord1 `before` chord2


boundChannels :: Motif (Simultanity a, b) -> Int
boundChannels (m0 :+: m1)   = max (boundChannels m0) (boundChannels m1)
boundChannels (Motif h _)   = foldl max 0 (map (polyphony . fst) h)
  where polyphony Silence   = 0
        polyphony (Sound _) = 1
        polyphony (a :=: b) = polyphony a + polyphony b

-- midiPlayer :: String -> Word8 -> [Word8] -> Player () [MidiEvent]
-- midiPlayer name instrument channels =
--   Player { name    = name
--          , perform = \_ -> []
--          }
