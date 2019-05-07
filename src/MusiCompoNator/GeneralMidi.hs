
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

pbSteps :: Integer -> Word8 -> Word8 -> Pitch -> [Word14]
pbSteps n key sense value = tail $
  scanl (const $ pbValue key sense . (+pivot)) 0 $
  map (\x -> (value - pivot) * (x / m)) [1..m]
  where pivot = fromIntegral key
        m     = n % 1

-- For now, we assume sense = 1, and bend upwards.
-- Later, we can have a 'pitch bend' operator {^o^} ?
bend :: Word8 -> Pitch -> Word14
bend sense p = pbValue (keyPress p) sense p

-- Which 'piano key' to press.
keyPress :: Rational -> Word8
keyPress q = fromIntegral $ (numerator q `div` denominator q)

-- Find the minimum common divisor of all beats.
-- Though, since (1 % 4) = bpm, we need to be able to express a quater.
beatSize :: Mesurable m => m Beat -> Integer
beatSize m = foldl lcm 4 (map denominator $ unmeasure m)

-- Compute the number of ticks to represent 't' using 'beatSize' beats.
ticks :: Integer -> Beat -> Integer
ticks d t = (d `div` denominator t) * numerator t

-- A shorthand voice event constructor
ve :: MidiVoiceEvent -> MidiEvent
ve = VoiceEvent RS_OFF

-- Bpm is implicitly (1 % 4) = bpm, so, we can compute tpm directly.
trackHead :: Int -> Word8 -> String -> [MidiMessage]
trackHead bpm sense title = [ (0, MetaEvent $ TextEvent SEQUENCE_NAME title)
                      , (0, MetaEvent $ SetTempo tpm)
                      ] ++ foldr (++) [] (map set $ [0..8] ++ [10..15])
  where tpm = (floor $ (10^6*60) / fromIntegral bpm)
        set = \c -> [(0, ve $ Controller c 101 00)     -- select rpn
                    ,(0, ve $ Controller c 100 00)     -- select pitch bend
                    ,(0, ve $ Controller c 006 sense)] -- adjust range in sense

-- All midi-files ends like this.
trackFoot :: [MidiMessage]
trackFoot = [(0, MetaEvent EndOfTrack)]

-- The costom part is a midi instrument bank number means (percussion?).
type MidiPlayer = Player (Word8, Bool) (Voice PhraseControl Prim Beat) [MidiEvent]

-- class Composition c where
--   add    :: String -> Voice d p b a -> c v t -> c v t
--   create :: String -> Player a v t  -> c v t -> Maybe (c v t)
--   remove :: String -> c v t -> c v t
--   alter  :: String -> ([v] -> [v]) -> c v t -> c v t
--   render :: c v t  -> Maybe t

-- general_midi :: MidiPlayer
-- general_midi =
--   Player { name    = "GM v. 1.0"
--          , perform = ? (:: (voice, a) -> Maybe target)
--          , costom  = (0, False)}
