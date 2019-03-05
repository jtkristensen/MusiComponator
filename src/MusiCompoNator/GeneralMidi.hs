
module MusiCompoNator.GeneralMidi where

import MusiCompoNator.Core
import MusiCompoNator.Composition
import ZMidi.Core
import Data.Ratio
import Data.Word

-- A midi-file is simply a file, listing the events.
listEvents :: Motif a -> [(Beat, a)]
listEvents (Motif h r) = zip r h
listEvents (m0 :+: m1) = listEvents m0 ++ listEvents m1

-- Each voice gets its own set of channels.
-- This limitings us to 16-note polyphony, but it also makes the voice
-- channels mutually exclusive, which saves us a lot of trouble {^_^}.
boundChannels :: Motif (Simultanity a, b) -> Int
boundChannels (m0 :+: m1)   = max (boundChannels m0) (boundChannels m1)
boundChannels (Motif h _)   = foldl max 0 (map (polyphony . fst) h)
  where polyphony Silence   = 0
        polyphony (Sound _) = 1
        polyphony (a :=: b) = polyphony a + polyphony b

-- Input  : key-pressed, pitchbend-sensitivity, target ratio
-- Outpus : 14-bit pitch-bend value (as interpreted in ZMidi.Core).
pbValue :: Word8 -> Word8 -> Pitch -> Word14
pbValue key sense value = fromIntegral $ n `div` d + 8192
  where k = (value - (fromIntegral key)) * (8191 % fromIntegral sense)
        n = numerator   k
        d = denominator k

-- For now, we assume sense = 1, and bend upwards.
-- Later, we can have a 'pitch bend' operator {^o^} ?
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

-- A shorthand voice event constructor
ve :: MidiVoiceEvent -> MidiEvent
ve = VoiceEvent RS_OFF

-- Bpm is implicitly (1 % 4) = bpm, so, we can compute tpm directly.
trackHead :: Int -> String -> [MidiMessage]
trackHead bpm title = [ (0, MetaEvent $ TextEvent SEQUENCE_NAME title)
                      , (0, MetaEvent $ SetTempo tpm)
                      ] ++ foldr (++) [] (map set $ [0..8] ++ [10..15])
  where tpm = (floor $ (10^6*60) / fromIntegral bpm)
        set = \c -> [(0, ve $ Controller c 101 00)  -- select rpn
                    ,(0, ve $ Controller c 100 00)  -- select pitch bend
                    ,(0, ve $ Controller c 006 01)] -- adjust range in se

-- All midi-files ends like this.
trackFoot :: [MidiMessage]
trackFoot = [(0, MetaEvent EndOfTrack)]

type MidiComposition = Composition () [(Beat, MidiEvent)]

-- Example:
triad = [i, iii, v]
melody :: Phrase ()
melody = (play   (arpeggio 1 triad `with` repeat en)) `before`
         (play $ [voicing 2 triad] `with` [wn])

-- midiPlayer :: String -> Word8 -> [Word8] -> Player () [MidiEvent]
-- midiPlayer name instrument channels =
--   Player { name    = name
--          , perform = \_ -> []
--          }

-- bpm -> [(title, runEvent)] -> zmidi.midifile.
-- trackSet bpm tracks =
--   let qs        = foldr (++) [] $ map (fst . snd) tracks
--       d         = beatSize qs (1 % 4)
--       tpb       = TPB $ fromIntegral $ d `div` 4
--       numTracks = fromIntegral (length tracks)
--       head'     = MidiHeader MF0 numTracks tpb
--       trkHd     = trackHead (floor $ (10^6*60) / fromIntegral bpm)
--       mkTrk (title, actions) = trkHd title ++ listAll d actions ++ trackFoot
--   in MidiFile head' $ map (MidiTrack . mkTrk . (second snd)) tracks
