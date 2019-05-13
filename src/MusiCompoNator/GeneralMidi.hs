
module MusiCompoNator.GeneralMidi where

import MusiCompoNator.Core
import MusiCompoNator.Composition
import ZMidi.Core
import Control.Monad.RWS
-- import Control.Arrow
import Data.Ratio
import Data.Word
import Data.List (sortBy)

data MidiInstrument = Piano Word8 | Percussion Word8 Word8
type Duration       = Beat

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

-- For now, we always bend upwards.
-- Later, we can have a 'pitch bend' operator {^o^} ?
bendw14 :: Word8 -> Pitch -> Word14
bendw14 sense p = pbValue (keyPress p) sense p

-- Which 'piano key' to press.
keyPress :: Rational -> Word8
keyPress q = fromIntegral $ (numerator q `div` denominator q)

-- Find the minimum common divisor of all beats.
-- Though, since (1 % 4) = bpm, we need to be able to express a quater.
beatSize :: Measurable m => m Beat -> Integer
beatSize m = foldl lcm 4 (map denominator $ unmeasure m)

-- Compute the number of ticks to represent 't' using 'beatSize' beats.
ticks :: Integer -> Beat -> Integer
ticks d t = (d `div` denominator t) * numerator t

-- A shorthand voice event constructor
ve :: MidiVoiceEvent -> MidiEvent
ve = VoiceEvent RS_OFF

-- A MIDI interpretation of PhraseControl.
data EffectMidi = MidiBend         EffectMidi EffectMidi
                | MidiTie          EffectMidi EffectMidi
                | MidiVolume Word8            EffectMidi
                | MidiCut    Beat             EffectMidi
                | MidiNotes  Beat  [Pitch]
                deriving (Show)

-- An event simply has time it happens, some data and some midi to perform.
data EventMidi   a = Event Beat a EffectMidi
                  deriving (Show)

instance Functor EventMidi where
  fmap f (Event b a e) = Event b (f a) e

-- A phrase is mapped to the events that has to happen, together
-- with a bound on the number of channels required at each event.
events :: Phrase2 -> [EventMidi Int]
events ph = resolvePhrasing 0 $ zip3 cs (map l ps) (unmeasure bs)
  where (cs, ps, bs)  = unPhrase ph
        l (Silence  ) = [ ]
        l (Sound  p ) = [p + 60] -- in MIDI, the middle c is 60.
        l (p1 :=: p2) = sortBy compare (l p1 ++ l p2)

resolvePhrasing :: Beat -> [([PhraseControl], [Pitch], Beat)] -> [EventMidi Int]
resolvePhrasing _ [ ] = []
resolvePhrasing b ((c, p, b')  : rest) =
  case resolvePhrasing (b + b') rest of
    [        ] -> return $ notes b c p b'
    phs        -> let (t, s) = depPhrasing c
                  in resolveDep t (notes b s p b') phs

notes :: Beat -> [PhraseControl] -> [Pitch] -> Beat -> EventMidi Int
notes b [] p b' = Event b (length p) (MidiNotes b' p)
notes b (Volume v : c) p b' =
  let Event b'' a e = notes b c p b'
      v'            = v * fromIntegral 0x7f
      (n, d)        = (fromIntegral $ numerator v', fromIntegral $ denominator v')
  in  Event b'' a $ MidiVolume (n `div` d) e
notes b (Legato : c) p b' =
  let Event b'' a e = notes b c p b'
  in  Event b'' a (MidiTie (MidiVolume 0 (MidiNotes 0 p)) e)
notes b (Staccato b''' : c) p b' =
  let Event b'' a e = notes b c p b'
  in  Event b'' a (MidiCut b''' e)
notes b (_ : c) p b' = notes b c p b'

depPhrasing :: [PhraseControl] -> ([PhraseControl], [PhraseControl])
depPhrasing cs = (filter (\c -> c == BendNext || c == TieNext) cs
                 ,filter (\c -> c /= BendNext && c /= TieNext) cs)

resolveDep :: [PhraseControl] -> EventMidi Int -> [EventMidi Int] -> [EventMidi Int]
resolveDep _  e [  ] = [e]
resolveDep [] e rest = e : rest
resolveDep (BendNext : c) (Event b i e) ((Event _ i' e') : rest) =
  resolveDep c (Event b (max i i') (MidiBend e e')) rest
resolveDep (TieNext : c) (Event b i e) ((Event _ i' e') : rest) =
  resolveDep c (Event b (max i i') (MidiTie e e')) rest
resolveDep (_ : c) e rest = resolveDep c e rest

durationOf :: EffectMidi -> Rational
durationOf (MidiBend  e e') = durationOf e + durationOf e'
durationOf (MidiTie   e e') = durationOf e + durationOf e'
durationOf (MidiVolume _ e) = durationOf e
durationOf (MidiCut   b e ) = min b (durationOf e)
durationOf (MidiNotes b  _) = b

allPitch, startPitch, endPitch :: EffectMidi -> [Pitch]
allPitch   (MidiBend   e e') = allPitch e ++ allPitch e'
allPitch   (MidiTie    e e') = allPitch e ++ allPitch e'
allPitch   (MidiVolume _ e ) = allPitch e
allPitch   (MidiCut    _ e ) = allPitch e
allPitch   (MidiNotes  _ p ) = p
startPitch (MidiBend   e _ ) = startPitch e
startPitch (MidiTie    e _ ) = startPitch e
startPitch (MidiVolume _ e ) = startPitch e
startPitch (MidiCut    _ e ) = startPitch e
startPitch (MidiNotes  _ p ) = p
endPitch   (MidiBend   _ e ) = endPitch e
endPitch   (MidiTie    _ e ) = endPitch e
endPitch   (MidiVolume _ e ) = endPitch e
endPitch   (MidiCut    _ e ) = endPitch e
endPitch   (MidiNotes  _ p ) = p

collectPitches :: [EventMidi a] -> [Pitch]
collectPitches es = concat $ map collect es
  where collect (Event _ _ e) = allPitch e

-- Derive from a scale, add instrument and percussion flag.
-- (All percussion needs to happen on channel 9).
phrase2eventMidi :: i -> [Phrase2] -> [EventMidi (i, Int)]
phrase2eventMidi i phs = map (fmap ((,) i)) $ concat $ map events phs

data MidiState =
  MPS { channels    :: [Word8]
      , pending     :: [(Beat, Word8)]
      , cursorB     :: Beat
      , bank        :: Word8
      , velocity    :: Word8
      , subdivision :: Integer
      , title       :: String
      , quater      :: Int     -- (1 % 4) = quater in bpm.
      , midi_events :: [EventMidi (MidiInstrument, Int)]
      }

defaultMidiPlayerState :: MidiState
defaultMidiPlayerState =
  MPS { channels    = [0..8] ++ [10..15]
      , pending     = mempty
      , cursorB     = 0
      , bank        = 0    -- grand piano
      , velocity    = 0x7f
      , subdivision = 4
      , title       = "untitled"
      , quater      = 120
      , midi_events = mempty
      }

midiVoice :: MidiInstrument -> Voice a -> MidiComposition ()
midiVoice i v = do
  s <- get
  k <- ask
  let (_, phs, _) = runVoice v k
  let m = maximum $ map beatSize phs
  put $ s { subdivision = lcm (subdivision s) m
          , midi_events = (phrase2eventMidi i phs) ++ midi_events s}

-- The output events are not sorted in delta-time.
type MidiComposition = RWS Scale [(Integer, MidiEvent)] MidiState

-- A midi composition runs to a midi-file.
runMidiComposition :: Scale -> MidiComposition a -> (MidiFile, a)
runMidiComposition s mc =
  (MidiFile (MidiHeader MF1 1 $ TPB tpb)
           [MidiTrack $ trackHead (quater ms) sense (title ms) ++
                        trk ++
                        trackFoot
           ], a)
  where (a, ms, w) = runRWS mc s defaultMidiPlayerState
        tpb        = fromIntegral $ ticks (subdivision ms) (1 % 4)
        deltaTime _ [             ] = []
        deltaTime b ((t, e) : rest) =
          (fromIntegral $ t - b, e) : deltaTime t rest
        ps    = collectPitches $ midi_events ms
        sense = floor (maximum ps - minimum ps) + 1
        trk   = deltaTime 0 $ sortBy (\x y -> compare (fst x) (fst y)) w
        trackHead bpm sense title =
          [ (0, MetaEvent $ TextEvent SEQUENCE_NAME title)
          , (0, MetaEvent $ SetTempo tpm)
          ] ++ (concat $ map set $ [0..8] ++ [10..15])
          where tpm = (floor $ (10^6*60) / fromIntegral bpm)
                set = \c -> [(0, ve $ Controller c 101 00)     -- select rpn
                            ,(0, ve $ Controller c 100 00)     -- select pb
                            ,(0, ve $ Controller c 006 sense)] -- adjust sense
        trackFoot = [(0, MetaEvent EndOfTrack)]

writeEvents :: [EventMidi (MidiInstrument, Int)] -> MidiComposition ()
writeEvents [                       ] = return ()
writeEvents ((Event b (i, n) e) : es) = do
  moveHead   b
  chs <- mapM allocCh $ take n $ repeat b
  writeEvent  b i chs e
  writeEvents es

writeEvent _ _ _ _ = return ()

putChannels :: [Word8] -> MidiComposition ()
putChannels chs = get >>= \s -> put $ s {channels = chs}

putCursor :: Beat -> MidiComposition ()
putCursor b = get >>= \s -> put $ s {cursorB = b}

lockUntil :: Word8 -> Beat -> MidiComposition ()
lockUntil ch b = get >>= \s -> put $ s {pending = (b, ch) : pending s}

-- Channels are allocated for a duration of time, then automatically freed.
allocCh :: Duration -> MidiComposition (Maybe Word8)
allocCh d = do
  s <- get
  case channels s of
    [         ] -> return Nothing
    (ch : rest) ->
      do putChannels rest
         ch `lockUntil` (cursorB s + d)
         Just <$> return ch

-- Free up a midi-channel.
freeCh :: Word8 -> MidiComposition ()
freeCh ch = (channels <$> get) >>= \chs -> putChannels $ ch : chs

-- Moving the player head means freeing midi channels that are no longer in use.
moveHead :: Beat -> MidiComposition ()
moveHead b = do
  s <- get
  put $ s { pending = [(b', ch) | (b', ch) <- pending s, b' > cursorB s]
          , cursorB = b }
  mapM_ freeCh $ [ch | (b', ch) <- pending s, b' <= cursorB s ]
