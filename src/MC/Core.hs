{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-|
  Module      : MC.Core
  Description : These are the fundamental building blocks in MC.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Core where

import Data.Bifunctor
import Control.Monad.State

-- * Harmonic abstractions.

infixr 5 :=:

-- | Discrete musical events happening at the same time are called
--   simultanities. 'Simultanity' @pitch@ is a single event where the
--   pitches produced by each event should be derivable from @pitch@.
--   We take 'Silence' to be the neutral element of musical events,
--   other simultanities are 'Sound's and simultanities that happen
--   simultaneoulsly (@s1 ':=:' s2@).
data Simultanity pitch =
    Silence
  | Sound       pitch
  | Simultanity pitch :=: Simultanity pitch
  deriving(Show)

instance Functor Simultanity where
  fmap _ Silence   = Silence
  fmap f (Sound a) = Sound $ f a
  fmap f (a :=: b) = fmap f a :=: fmap f b

instance Semigroup (Simultanity pitch) where
  (<>) = (:=:)

instance Monoid (Simultanity pitch) where
  mempty  = Silence
  mappend = (<>)

instance Foldable Simultanity where
  foldMap _ (Silence) = mempty
  foldMap f (Sound p) = f p
  foldMap f (a :=: b) = foldMap f a <> foldMap f b

-- | Motifs, melodies and phrases are simply sequences of simultanities.
data Sequence pitch = Seq [Simultanity pitch]

instance Functor Sequence where
  fmap f (Seq xs) = Seq $ map (fmap f) xs

instance Semigroup (Sequence pitch) where
  (Seq xs) <> (Seq ys) = Seq $ xs ++ ys

instance Monoid (Sequence pitch) where
  mempty  = Seq []
  mappend = (<>)

-- | We measure absolute pitch in semitones relative to the middle c.
--   So, 0 corresponds to C4 (261.626 hertz).
type Semitone = Rational
-- | Pitches are denoted by their distance to the middle c in 'Semitones'.
type Pitch    = Semitone

-- | A 'Scale' in modern music theory, is defined a 'Pitch'
--   and a sequence of 'Semitone' intervals spanning exactly 1 octave in total.
--   As such, these intervals define a sequence of 'Pitch'es within each octave,
--   and we call these 'Pitch'es the memebers of the 'Scale'.
data Scale = Scale Pitch [Semitone]
           deriving (Show)

-- | Amongst musicians and composers, the abillity to express oneself in
--   terms of absolute 'Pitch' is rather uncommon.
--   Instead, we express @'Relative' pitch@ as a @pitch@ in the context
--   of some underlying 'Scale'.
type Relative pitch = State Scale pitch

-- | Combining 'Relative' 'Pitche's @p1@ and @p2@, ammounts to deriving the
--   mode from which @p1@ was drawn, and then derive @p2@ from that mode,
--   and this happens to be an associative operation.
instance Semigroup (Relative Pitch) where
  p1 <> p2 = mode (relMode p1) p2

-- | The 'Relative' 'Pitch' that just assumes the 'root' note of a 'Scale',
--   may be viewed as the neutral element for combining pitches since
-- >  id p = mempty <> p
instance Monoid (Relative Pitch) where
  mempty  = get >>= \s -> return (root s)
  mappend = (<>)

-- | Even though the user may decide on encoding 'Pitch' differently than
--   using 'Semitone' steps, we mostly just use the above definition of
--   'Pitch'
type RPitch = Relative Pitch

-- | A 'Chord' is a named collection of a subset of the members of a 'Scale'.
--   As such, 'Relative' 'Pitch'es and 'Relative' 'Chord's are exactly the same.
type Chord  = Relative Pitch

-- * Rhythmical abstractions.

infixr 5 :|: -- bar
infixr 5 :-: -- tie

-- | A 'Rhythm' is a collection of beats structured in measures.
--   A @beat@ does not need to be entirely contained within a measure,
--   and the ':-:' constructor denotes a combination of measures
--   where the last @beat@ in one measure is part of the first beat
--   in the next. If this is not the case, two measures are separated
--   by a bar (':|:').
data Rhythm beat =
    Measure [beat]
  | (Rhythm beat) :|: (Rhythm beat)
  | (Rhythm beat) :-: (Rhythm beat)
    deriving(Show)

-- | Mapping a function onto a 'Rhythm' simply ammounts to applying the function
--   to the @beat@s it consists of.
instance Functor Rhythm where
  fmap f (Measure bs) = Measure $ map f bs
  fmap f (r1 :|: r2)  = fmap f r1 :|: fmap f r2
  fmap f (r1 :-: r2)  = fmap f r1 :-: fmap f r2

-- | Rhythms may of course be combined sequentially, and this operation
--   happens to be associative.
instance Semigroup (Rhythm b) where
  (<>) = (:|:)

-- | Because a 'Rhythm' is 'Measurable', we may take the @0@ 'Duration'
--   'Rhythm' as neutral element for combining 'Rhythm's.
instance (Num b, Ord b) => Monoid (Rhythm b) where
  mempty        = measure []
  mappend r1 r2 = measure $ unmeasure r1 <> unmeasure r2

-- | BPM is short for @beats per minute@.
type BPM      = Int         -- Beats per minute

-- | 'Beat's are essentially subdivisions of a meter.
type Beat     = Rational

-- | A more descriptive word for 'Beat' may be 'Duration' in some settings.
type Duration = Beat

-- | Commonly, we construct 'Rhythm's from 'Beat's.
type Rhythm1  = Rhythm Beat

-- | A 'Signature' is essantially a description of the way a 'Rhythm' is structured
--   relative to the meter.
data Signature a = Times Int a | Shift (Signature a) (Signature a)

instance Show a => Show (Signature a) where
  show (Times i  m) = show i ++ "x" ++ "(" ++ show m ++ ")"
  show (Shift s s') = show s ++ " || " ++ show s'

instance Functor Signature where
  fmap f (Times n  a ) = Times n $ f a
  fmap f (Shift s1 s2) = Shift (fmap f s1) (fmap f s2)

instance Semigroup (Signature a) where
  (<>) = Shift

-- | We say that a thing @r@ is 'Timed', if it has a time 'Signature'.
--   We also want to be able to change this 'Signature', and in
--   doing so, restructuring @r@ to fit the 'Signature'.
class Timed r where
  withSignature :: (Num a, Ord a) => (Signature a) -> (r a) -> (r a)
  signature     :: (Num a, Ord a) => (r a) -> (Signature a)

-- | A 'Rhythm' is in particular 'Timed', because its structure relative to
--   the meter is a time 'Signature' on the 'Beat's it consists of.
instance Timed Rhythm where
  withSignature s r = aquire [] (meters s) (unmeasure r)
    where
      meters (Times n   m) = map (const m) [1..n]
      meters (Shift s0 s1) = meters s0 ++ meters s1
      aquire m _        [      ] = measure (reverse m)
      aquire m [      ] _        = measure (reverse m)
      aquire m (q : qs) (b : bs) =
        case q `compare` b of
          EQ -> measure (reverse $ b : m) :|: aquire [     ] qs           bs
          LT -> measure (reverse $ q : m) :-: aquire [     ] qs  (b - q : bs)
          GT ->                               aquire (b : m) (q - b : qs) bs
  signature = collect . meters
    where
      meters (Measure      bs ) = [Times 1 $ (sum bs)]
      meters (r1 :|: r2)        = meters r1 ++ meters r2
      meters (r1 :-: r2)        = meters r1 ++ meters r2
      collect [s] = s
      collect (Times n k : Times m k' : s) =
        if   k == k'
        then collect (Times (n + m) k : s)
        else Shift (Times n k) $ collect (Times m k' : s)
      collect _ = error "impossible (by construction)."

-- | We say that a thing @m@ is 'Measurable', if it can be construted from
--   a list of numbers.
class Measurable m where
  measure       :: (Num a, Ord a) => [a] -> m a
  unmeasure     :: (Num a, Ord a) => m a -> [a]

-- | A 'Rhythm' is a way of structuring a list of 'Beat's, hence it is
--  'Measurable' almost by definition.
instance Measurable Rhythm where
  measure = Measure
  unmeasure (Measure bs)   = bs
  unmeasure (r1 :|: r2)    = unmeasure r1 ++ unmeasure r2
  unmeasure (r1 :-: r2)    = unmeasure r1 `tie` unmeasure r2
    where tie []  r2      = r2
          tie [x] (h : t) = (x + h : t)
          tie (x : xs) t  = x : tie xs t

-- * Abstractions on the combination of harmony and rhythm.

infixr 4 :<: -- motif.
infixr 3 :+: -- sequntial composition.

-- | A 'Phrase' is a the smallest construction of complete musical meaning.
--   It may either be a motif, which consists of a 'Sequence' of musical events
--   together with a 'Rhythm' that specifies when the events should happen relative
--   to the meter. More complicated 'Phrase's may be constructed be sequencing (':+:')
--   several simpler 'Phrase's.
--   Additionally, a 'Phrase' may be annotated with a 'Control' stuctures that
--   describe how the events should be performed (sometimes called phrasing),
--   or by a time 'Signature' that describes the structure of its underlying
--   rhythm.
data Phrase a b =
    (Sequence a  ) :<: (Rhythm   b)
  | (Phrase   a b) :+: (Phrase a b)
  | Ctrl Control       (Phrase a b)
  | Sig  (Signature b) (Phrase a b)

-- | A 'Percentage' is represented by a rational number in the range @[0, 1]@.
type Percentage = Rational

-- | The phrasing that can be controlled currently, is the 'Velocity'
--   at which a musical note should be played, and the actual 'Duration'
--   with that it is sounding for. A 'Sequence' of notes that are restricted
--   to some particular 'Duration' are said to be phrased in 'Staccato'.
data Control = Staccato  Duration
             | Legato
             | Velocity  Percentage
             | Modulator Percentage
             | Reverb    Percentage

-- | Since a 'Phrase' consists of two parts, a 'Rhythm' and a 'Sequence'
--   of 'Pitches', it is naturally a bi-functor on those parts.
instance Bifunctor Phrase where
  bimap f g (harmony :<: rhythm) = (fmap f harmony) :<: (fmap g rhythm)
  bimap f g (ph1     :+: ph2   ) = (bimap f g ph1)  :+: (bimap f g ph2)
  bimap f g (Ctrl c ph         ) = Ctrl c $ bimap f g ph
  bimap f g (Sig  s ph         ) = Sig (fmap g s) $ bimap f g ph

-- | A 'Phrase' consists amongst other things of a 'Rhythm',
--   and since 'Rhythm's can be measured, then so can 'Phrase's
instance Measurable (Phrase a) where
  measure      = foldr1 (<>) . map rest
  unmeasure ph = let (_, _, r) = unPhrase ph in unmeasure r

-- | A 'Phrase' consists amongst other things of a 'Rhythm',
--   and since 'Rhythm's can are 'Timed', then so are 'Phrase's
instance Timed (Phrase a) where
  withSignature s  = liftR (withSignature s)
  signature     ph = let (_, _, r) = unPhrase ph in signature r

-- | Sequential combinations of phrases happens to be an asscociative
--   relation.
instance Semigroup (Phrase a b) where
  (<>) = (:+:)

-- | An 'AbstractPhrase', is a phrase that can be 'derive'd from a 'Scale',
--   but does not have any concrete meaning until that happens
type AbstractPhrase = Phrase (Relative Pitch) Beat

data VoiceState =
  VS { vsPhrases :: [Phrase Pitch Beat]
     , vsCursor  :: Beat
     , vsKey     :: Scale
     }

-- | A 'Voice' is a collection of 'Phrase's that should be played at the same time.
--   Additionally, a 'Voice' must be specified in some 'Scale', that give absolute
--   meaning to the 'Relative' 'Pitche's in 'AbstractPhrase's
type Voice = State (VoiceState)

runVoice :: (Voice a) -> Scale -> (a, [Phrase Pitch Beat])
runVoice v s = (a, vsPhrases vs)
  where (a, vs) = runState v vs1
        vs1 =
          VS { vsPhrases = []
             , vsCursor  = 0
             , vsKey     = s
             }

-- * Utility functions for Voice.

-- | Shifts the notes in a voice by @n@ Semitones.
transpose :: Semitone -> Voice ()
transpose n = do
  s <- get
  put $ s {vsPhrases = map (liftH $ fmap (+n)) $ vsPhrases s}

-- | Returns the scale from which a voice is currently being derived.
tonality :: Voice Scale
tonality = do
  s <- get
  return $ vsKey s

-- | Playing an absolute phrases, corresponds to writing it down
--   with out caring about the underlying scale.
playAbsolutePhrase :: Phrase Pitch Beat -> Voice ()
playAbsolutePhrase ph = do
  s <- get
  let b = vsCursor s
  put $ s { vsPhrases = rest b <> ph : vsPhrases s
          , vsCursor  = b + duration ph
          }

-- | Playing an abstract phrase, corresponds to deriving its absolute
--   pitches from the scale in which the voice should be played.
play :: AbstractPhrase -> Voice ()
play aPh = do
  s <- get
  let ph = liftH (fmap $ derive $ vsKey s) $ aPh
  playAbsolutePhrase ph

-- | A convinient method for playing multiple phrases at the same time.
more :: [AbstractPhrase] -> Voice ()
more [        ] = return ()
more (ph : phs) = do
  s1 <- get
  play ph
  s2 <- get
  put $ s2 {vsCursor = vsCursor s1}
  more phs
  s3 <- get
  put $ s3 {vsCursor = max (vsCursor s2) (vsCursor s3)}

-- | An Abstract phrase may be mapped in to a relative range of
--   pitches inside a voice, sometimes called the register of a voice.
between :: RPitch -> RPitch -> AbstractPhrase -> Voice AbstractPhrase
between low high ph = do
  s <- tonality
  let (a, b) = (derive s low, derive s high)
  return $ liftH (fmap $ fmap $ bounded a b) ph

-- | The cursor in VoiceState increments whenever a phrase is added.
--   So running two voices after each other automatically combines
--   the phrases sequentially.
instance Semigroup (Voice a) where
  (<>) = (>>)

-- * Utility functions.

-- | Transpose a 'Pitch' up/down an octave (12 'Semitone's).
up, down :: Pitch -> Pitch
up    = (+)      12
down  = flip (-) 12

-- | Maps 'Pitche's into enharmonically equivalent 'Pitch'es within
--   a given range (so long as the range spans at least 12 'Semitone's).
bounded :: Pitch -> Pitch -> (Pitch -> Pitch)
bounded low high p =
  case (low `compare` p, high `compare` p) of
    (LT, LT) -> bounded low high (max low  (down p))
    (GT, GT) -> bounded low high (min high (up p))
    _        -> p

-- | We call the 'Pitch' at which a 'Scale' starts the 'root' note.
root :: Scale -> Pitch
root (Scale r _) = r

-- | The inversions of a 'Scale', are scales that produce the same members
--   from different 'root' 'Pitche's.
invertr, invertl :: Scale -> Scale

invertr (Scale r steps) = Scale (r + s) (teps ++ [s])
  where (s, teps) = (head steps, tail steps)

invertl (Scale r steps) = Scale (r - s) (s : step)
  where (step, s) = (init steps, last steps)

-- | Chooses the inversion rooted at the member with some index in a 'Scale'
index :: Int -> Scale -> Scale
index = step . (+1)

-- | Musicians then to 'index' things from 1. As such
--
-- > step 1 s = index 0 s
step :: Int -> Scale -> Scale
step 1 s = s
step n s = if   n < 1
           then step (n + 1) (invertl s)
           else step (n - 1) (invertr s)

-- | The chromatic 'Scale' is simply every 'Semitone' whole step in every octave.
chromatic :: Pitch -> Scale
chromatic p = Scale p $ take 12 $ repeat 1

-- | Provided some 'Scale' @s@, we can 'derive' an absolute 'Pitch' from a
--   'Relative' one.
derive :: Scale -> Relative pitch -> pitch
derive s rp = fst $ runState rp s

-- | Returns the 'mode' that a 'Relative' 'Pitch' is going to
--   be drawn from, if it was to be drawn from a particular 'Scale'.
relMode :: Relative Pitch -> (Scale -> Scale)
relMode rp s = Scale p t
  where (p, Scale _ t) = runState rp s

-- | Modifies the 'Scale' that a @'Relative' pitch@ is drawn from.
mode :: (Scale -> Scale) -> Relative pitch -> Relative pitch
mode f rp = do
  s <- get
  put (f s)
  rp

-- | Modifies a @'Relative' pitch@ to be drawn from the scale underlying a 'Chord'
chord2mode :: Chord -> Relative pitch -> Relative pitch
chord2mode = mode . relMode

-- | Returns the duration of a 'Measurable' thing.
duration :: (Num a, Ord a, Measurable m) => m a -> a
duration = sum . unmeasure

-- | The simplest 'Rhythm' that we can think of. A single beat.
beat :: (Num b, Ord b) => b -> Rhythm b
beat b = measure [b]

-- | Constructs an 'AbstractPhrase' that consists only of a rhythm.
--   (usefull when writing drum patterns).
rhythm :: Rhythm1 -> AbstractPhrase
rhythm r = foldr1 (<>) $ map (flip note mempty) $ unmeasure r

-- | Puts a list of @pitch@es into the single event 'Sequence'.
simultanity :: [pitch] -> Sequence pitch
simultanity = Seq . return . sim
  where sim [      ] = Silence
        sim [p     ] = Sound p
        sim (p : ps) = Sound p :=: sim ps

-- | Returns a 'Pitch' into the single pitch 'Sequence'.
pitch :: pitch -> Sequence pitch
pitch = simultanity . return

-- | Returns a drum hit (does not care about pitch)
hit :: Sequence (Relative Pitch)
hit = line [return 1]

-- | The single event 'Sequence' consisting of 'Silence'.
silence :: Sequence pitch
silence = Seq $ return mempty

-- | A melodic line is a 'Sequence' of single note @pitche@s
line :: [pitch] -> (Sequence pitch)
line = foldr ((<>) . pitch) mempty

-- | An 'arpeggio' is a line drawn from the 'Relative' 'mode' underlying a chord.
arpeggio :: [Relative pitch] -> Chord -> Sequence (Relative pitch)
arpeggio ps p = fmap (chord2mode p) $ line ps

-- | A voicing is a particular way of drawing the @pitch@es from a 'Chord'.
voicing :: [Relative pitch] -> Chord -> Sequence (Relative pitch)
voicing ps p = fmap (chord2mode p) $ simultanity ps

-- | Constructing a 'Phrase' from its underlying structures.
phrase :: (Num b, Ord b) => ([[Control]], Sequence a, Rhythm b) -> Phrase a b
phrase (css, Seq p, r) =  Sig (signature r) $
  foldr1 (:+:) $ map f $ zip3 css p (unmeasure r)
  where f (c, p', b) = ctrls c (Seq [p'] :<: measure [b])
        ctrls c e    = foldr (\f x -> f x) e (fmap Ctrl c)

-- | Deconsruts a 'Phrase' into 'Control', harmony and 'Rhythm'.
unPhrase :: (Num b, Ord b) => Phrase a b -> ([[Control]], Sequence a, Rhythm b)
unPhrase (Ctrl c' ph ) = let (c, p, r) = unPhrase ph in (map (c':) c, p, r)
unPhrase (Sig  s  ph ) = let (c, p, r) = unPhrase ph in (c, p, withSignature s r)
unPhrase (ph1 :+: ph2) = unPhrase ph1 <> unPhrase ph2
unPhrase (Seq h :<: r  ) = (map (const []) h', Seq h', measure r')
  where (h', r') = unzip $ zip h (unmeasure r)

-- | Lift functions on 'Sequence' to 'Phrase's.
liftH :: (Num r, Ord r) => (Sequence a -> Sequence b) -> Phrase a r -> Phrase b r
liftH f ph = phrase (cs, f sp, r) where  (cs, sp, r) = unPhrase ph

-- | Lift functions from 'Rhythm's to 'Phrase's.
liftR :: (Num a, Ord a, Num b, Ord b) =>
         (Rhythm a -> Rhythm b) -> Phrase p a -> Phrase p b
liftR f ph = phrase (cs, sp, f r) where  (cs, sp, r) = unPhrase ph

-- | The single note 'Phrase'.
note :: (Num b, Ord b) => b -> p -> Phrase p b
note b p = pitch p :<: beat b

-- | The silent 'Phrase'.
rest :: (Num b, Ord b) => b -> Phrase p b
rest b  = silence :<: beat b

-- | Apply 'Control's to a phrase.
modifyCtrl :: Control -> Phrase p b -> Phrase p b
modifyCtrl = Ctrl

-- | Change the 'Velocity' at which a phrase should be played.
velocity :: Percentage -> Phrase p b -> Phrase p b
velocity q = modifyCtrl $ Velocity q

-- | Sets on a bound on the sounding 'Duration' of the notes in a 'Phrase'.
staccato :: Duration -> Phrase p b -> Phrase p b
staccato b = modifyCtrl $ Staccato b

-- | Play the notes more connected, and at a more gentle velocity.
legato :: Phrase p b -> Phrase p b
legato = modifyCtrl $ Legato

modulate :: Percentage -> Phrase p b -> Phrase p b
modulate = modifyCtrl . Modulator

reverb :: Percentage -> Phrase p b -> Phrase p b
reverb = modifyCtrl . Reverb

-- | Repeats a 'Phrase'.
times :: Int -> Phrase p b -> Phrase p b
times n ph = foldr (<>) ph $ take (n - 1) $ repeat ph

-- | Changes the tone material from which the 'Relative' 'Scale' in annotated
--  'AbstractPhrase' should be drawn.
inKey :: AbstractPhrase -> Scale -> AbstractPhrase
inKey aPh s = liftH (fmap $ mode $ const s) $ aPh

-- | Shift an 'AbstractPhrase' by a number of octaves.
octave :: Int -> AbstractPhrase -> AbstractPhrase
octave 0 aPh = aPh
octave n aPh =
  if   n < 0
  then octave (n + 1) $ liftH (fmap (fmap down)) aPh
  else octave (n - 1) $ liftH (fmap (fmap   up)) aPh
