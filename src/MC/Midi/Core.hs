{-|
  Module      : MC.Midi.Core
  Description : Functions for exporting compositions to MIDI.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Midi.Core where

import MC.Core
import ZMidi.Core
import Control.Monad.RWS
import Control.Monad.State
import Data.Ratio
import Data.Word
import Data.Maybe (catMaybes)
import Data.List  (sortBy)


-- | In the general midi standard, most things are represented by a 7-bit value.
type MidiVelocity   = Word8
type MidiPitch      = (Word8, Word8, Word14)
-- | (Key pressed, Pitch bend modulator sensitivity, Pitch bend setting).
data MidiInstrument = Piano Word8 | Percussion Word8 Word8

-- | Currently, an event is simply a 'Simultanity' of events
--   that should be played at a specific 'Beat' and last for a particular
--   'Duration'.
data EventMidi a = Notes Beat Duration a (Simultanity MidiPitch)

instance Functor EventMidi where
  fmap f (Notes b d a s) = Notes b d (f a) s

instance Eq (EventMidi a) where
  e1 == e2 = eWhen e1 == eWhen e2
    where eWhen (Notes b _ _ _) = b

-- | We impose a parital order on 'EventMidi', where events are "smaller"
--   if they happen earlier in time.
instance Ord (EventMidi a) where
  e1 `compare` e2 = eWhen e1 `compare` eWhen e2
    where eWhen (Notes b _ _ _) = b

-- Future Work:
-- - Include more dynamic events, like meta events and controller events,
--   like when the sustainpedal should be pressed.
-- - Also, since we have already utilised the pitch bend modulator, we could
--   add an actual pitch bend operation for guitar bends {~_^}.

-- | In general midi, an instrument is specified by a tone bank number
--   (See Midi.Instrument).
toneBank :: MidiInstrument -> Word8
toneBank (Piano      bank  ) = bank
toneBank (Percussion bank _) = bank

-- | Converts the Pitch from MC.Core, into the MidiPitch type.
--   Note that percussion has a @keypress@ value in midi, but
--   I decided to embed that in the instrument itself.
pitch2midiPitch :: MidiInstrument -> Pitch -> MidiPitch
pitch2midiPitch i p =
  case i of
    (Percussion _ k') -> (k', s + 1, 8192)
    _                 -> (k + 60, s + 1, pb)
  where k  = keyPress p
        s  = 1 -- future work.
        pb = pbValue k s p

percentage2word8 :: Percentage -> Word8
percentage2word8 q = fromIntegral $ n `div` d
  where v = fromIntegral 0x7f * q
        (n, d) = (numerator v, denominator v)

-- | If no velocity is specified we just hit as hard as we can.
ctrls2velocity :: [Control] -> MidiVelocity
ctrls2velocity [              ] = 0x7f
ctrls2velocity (Velocity q : _) = percentage2word8 q
ctrls2velocity (_ : ctrls) = ctrls2velocity ctrls

-- | The duration of an event is just the number of ticks that encodes
--   its actual duraion, unless it is played staccato, and the user
--   specified something else.
ctrls2duration :: Duration -> [Control] -> Duration
ctrls2duration d [              ] = d
ctrls2duration _ (Staccato d : _) = d
ctrls2duration d (_ : ctrls     ) = ctrls2duration d ctrls

-- | Rewrites a phrase to a list of midi events.
events :: MidiInstrument -> Phrase Pitch Beat -> [EventMidi [Control]]
events i ph = resolve 0 $ zip3 cs ps (unmeasure bs)
  where (cs, Seq ps, bs) = unPhrase ph
        resolve _  [                ] = []
        resolve b' ((c, p, b) : rest) =
          Notes b' b c (fmap (pitch2midiPitch i) p) :
          resolve (b + b') rest

-- | Rewrites a list of phrases to a combined list of midi events.
phrase2eventMidi :: MidiInstrument ->
                    [Phrase Pitch Beat] ->
                    [EventMidi (MidiInstrument, [Control])]
phrase2eventMidi i phs = map (fmap ((,) i)) $ concat $ map (events i) phs

-- | A midi composition is a manager for the midi events.
--   It also keeps track of the smallest subdivision relative to the meter
--   that it has encountered (later, we need this information to decide
--   on a precise tempo for midi output).
type MidiComposition = RWS Scale [(Integer, MidiEvent)] MidiState
data MidiState =
  MPS { msCursor      :: Beat
      , msInstrument  :: MidiInstrument
      , msSubdivision :: Integer
      , msEvents      :: [EventMidi (MidiInstrument, [Control])]
      }

-- | Play future phrases on this instrument.
instrument :: MidiInstrument -> MidiComposition ()
instrument i = do
  s <- get
  put $ s { msInstrument = i }

on :: Voice a -> MidiInstrument -> MidiComposition a
on v i = instrument i >>  voice2midi v

-- | The simplest composition is a voice.
voice2midi :: Voice a -> MidiComposition a
voice2midi v = do
  s           <- get
  k           <- ask
  let i        = msInstrument s
  let (a, phs) = runVoice v k
  let sub      = msSubdivision s
  put $ s { msEvents      = msEvents s ++ phrase2eventMidi i phs
          , msSubdivision = lcm sub $ foldl lcm 4 $ map beatSize phs
          }
  return a

-- * The rest is IO.

-- | Which piano key to press, to obtain a particular pitch.
keyPress :: Pitch -> Word8
keyPress q = fromIntegral $ (numerator q `div` denominator q)

-- | Input  : key-pressed, pitchbend-sensitivity, target ratio
--   Output : 14-bit pitch-bend value (as interpreted in ZMidi.Core).
pbValue :: Word8 -> Word8 -> Pitch -> Word14
pbValue key sense value = fromIntegral $ n `div` d + 8192
  where k = (value - (fromIntegral key)) * (8192 % fromIntegral sense)
        n = numerator   k
        d = denominator k

-- | Computes the pitchbend modulator position based on the played key,
--   the pitch bend modulators sensitivity settings, and the pitch
--   that we want to produce.
bendw14 :: Word8 -> Pitch -> Word14
bendw14 sense p = pbValue (keyPress p) sense p

-- | Find the least common multiplier amoung the beats in a phrase
--   (including those imposed by staccato etc..).
beatSize :: Phrase p Beat -> Integer
beatSize ph = stops
  where beats          = collect 4 $ unmeasure rh
        stops          = collect beats $ map (ctrls2duration (1 % 4)) ctrls
        (ctrls, _, rh) = unPhrase ph
        collect b      = foldl lcm b . map denominator

-- | Compute the number of ticks to represent @t@ using @beatSize@ beats.
ticks :: Integer -> Beat -> Integer
ticks d t = (d `div` denominator t) * numerator t

-- | A shorthand voice event constructor
ve :: MidiVoiceEvent -> MidiEvent
ve = VoiceEvent RS_OFF

-- -- Channel Allocation ------------------------------------------
-- In the General Midi (GM) Specification, a track has 16 channels,
-- One of which is dedicated to percussion sounds. A ChannelAllocator
-- is just a State keeps track of these channels.
-- Because we represented pitch by rational numbers measured in
-- semitones, we will have to adjust the pitch of each note using
-- the pitch-bend-modulator (also in GM), and for this reason
-- no two notes can sound on the same channel at the same time.
-- ----------------------------------------------------------------

data ChannelState     =
  CS { chPercussion :: Word8
     , chAvailable  :: [Word8]
     , chQueue      :: [(Word8, Beat)]
     , chCursor     :: Beat
     }
type ChannelAllocator = State ChannelState

-- | Allocate a channel for a particular midi-instrument, until a particular beat.
allocCh :: MidiInstrument -> Beat -> ChannelAllocator (Maybe Word8)
allocCh i b = do
  s <- get
  case (chAvailable s, i) of
    ([         ], Piano _       ) -> return Nothing
    (_          , Percussion _ _) -> Just <$> (return $ chPercussion s)
    ((ch : rest), _             ) ->
      do put s { chAvailable  = rest
               , chQueue      = (ch, b) : chQueue s
               }
         Just <$> return ch

-- | Moving the player head, then free channels that are no longer in use.
moveHead :: Beat -> ChannelAllocator ()
moveHead b = do
  s  <- get
  put $ s { chQueue     = [(ch, b') | (ch, b') <- chQueue s, b < b']
          , chCursor    = b
          , chAvailable = [ch | (ch, b') <- chQueue s, b' <= b] ++ chAvailable s}

-- | Construct a channel allocator.
allocateChs :: [EventMidi (MidiInstrument, v)] ->
               ChannelAllocator [EventMidi ([Maybe Word8], MidiInstrument, v)]
allocateChs [                            ] = return []
allocateChs ((Notes b d (i, v) ps) : rest) = do
  moveHead b
  chs <- mapM (allocCh i) $ take (length ps) $ repeat (b + d)
  evs <- allocateChs rest
  return $ Notes b d (chs, i, v) ps : evs

-- | Run a channel allocator.
runAllocator :: ChannelAllocator a -> a
runAllocator ca = fst $ runState ca cs0
  where cs0 = CS { chPercussion = 9
                 , chAvailable  = [0..8] ++ [10..15]
                 , chQueue      = []
                 , chCursor     = 0
                 }

ms0 :: MidiState
ms0 =
  MPS { msCursor      = 0
      , msInstrument  = Piano 0x00
      , msSubdivision = 4
      , msEvents      = []
      }

-- | A midi composition runs to a midi-file.
midiMC2File :: String -> BPM -> Relative Pitch -> (Pitch -> Scale) ->
               MidiComposition a -> IO ()
midiMC2File path bpm rp f mc = writeMidi path $
  MidiFile (MidiHeader MF1 1 $ TPB tpb)
           [MidiTrack $ trackHead ++
                        trk ++
                        trackFoot
           ]
  where s          = f (derive (chromatic 0) rp)
        (_, ms, w) = runRWS (mc >> closeMC) s ms0
        tpb        = fromIntegral $ ticks (msSubdivision ms) (1 % 4)
        deltaTime _ [             ] = []
        deltaTime b ((t, e) : rest) =
          (fromIntegral $ t - b, e) : deltaTime t rest
        trk       = deltaTime 0 $ sortBy (\x y -> compare (fst x) (fst y)) w
        trackHead =
          [(0, MetaEvent $ SetTempo $ (floor $ (10^6*60) / fromIntegral bpm)) ]
        trackFoot =
          [(0, MetaEvent EndOfTrack)]

-- | A couple of helper functions for midiMC2File:

closeMC :: MidiComposition ()
closeMC = do
  s <- get
  writeEvents $ runAllocator $ allocateChs $ sortBy compare $ msEvents s

writeEvents :: [EventMidi ([Maybe Word8], MidiInstrument, [Control])] ->
               MidiComposition ()
writeEvents [                                 ] = return ()
writeEvents ((Notes b d (chs, i, c) ps) : rest) = do
  s <- get
  let f    = ticks (msSubdivision s)
  let ps'  = foldMap return ps
  let chs' = catMaybes chs
  let d'   = b + ctrls2duration d c
  mapM (setInstrument  (f b) i)     chs'
  mapM (resolve (f b) c) $ chs'
  mapM (writeOn  (f b) $ ctrls2velocity c) $ zip chs' ps'
  mapM (writeOff (f $ d')) $ zip chs' ps'
  mapM (unResolve (f b) c) $ chs'
  writeEvents rest

resolve :: Integer -> [Control] -> Word8 -> MidiComposition ()
resolve _ [             ] _ = return ()
resolve t (Legato : rest) ch = do
  tell [(t, ve $ Controller ch 68 0x7f)]
  resolve t rest ch
resolve t (Modulator p : rest) ch = do
  tell [(t, ve $ Controller ch 1 $ percentage2word8 p)]
  resolve t rest ch
resolve t (Reverb p : rest) ch = do
  tell [(t, ve $ Controller ch 91 $ percentage2word8 p)]
  resolve t rest ch
resolve t (_      : rest) ch =
  resolve t rest ch

unResolve :: Integer -> [Control] -> Word8 -> MidiComposition ()
unResolve _ [             ] _ = return ()
unResolve t (Legato : rest) ch = do
  tell [(t, ve $ Controller ch 68 0x00)]
  resolve t rest ch
unResolve t (Modulator _ : rest) ch = do
  tell [(t, ve $ Controller ch 1 0x00)]
  resolve t rest ch
unResolve t (Reverb _ : rest) ch = do
  tell [(t, ve $ Controller ch 91 0x00)]
  resolve t rest ch
unResolve t (_      : rest) ch =
  resolve t rest ch

setInstrument :: Integer -> MidiInstrument -> Word8 -> MidiComposition ()
setInstrument t i ch = tell [(t, ve $ ProgramChange ch $ toneBank i)]

writeOn :: Integer -> MidiVelocity -> (Word8, MidiPitch) -> MidiComposition ()
writeOn t v (ch, (k, s, pb)) = tell $ map ((,) t . ve)
  [ Controller ch 101 00  -- select rpn.
  , Controller ch 100 00  -- select pb.
  , Controller ch 006 s   -- adjust sense.
  , PitchBend  ch pb      -- adjust pb controller.
  , NoteOn     ch k v   ] -- press key.

writeOff :: Integer -> (Word8, MidiPitch) -> MidiComposition ()
writeOff t (ch, (k, _, _)) = tell [(t, ve $ NoteOff ch k 0)]

