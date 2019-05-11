{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where
import MusiCompoNator.Core
import Control.Monad.State
import qualified Data.Map as Map

-- * Harmonic construction.

-- | Everything is constructed from primitive abstractions on scales.
data Prim =
    Voicing [(Scale -> Pitch)]
  | Mode     (Scale -> Scale) Prim

-- | Single concrete pitch (not recommended).
absPitch :: Rational -> (Sequence Prim)
absPitch q = return (Voicing . return $ const q)

-- | Single pitch drawn from some scale.
pitch :: (Scale -> Pitch) -> (Sequence Prim)
pitch i = return (Voicing . return $ i)

-- | No pitch at all.
silence :: Sequence Prim
silence = return $ Voicing []

-- | Transposing a primitive harmonic structure.
transpose :: Pitch -> Sequence Prim -> Sequence Prim
transpose p = fmap $ Mode $ fmap (+p)

-- | Shift a primitiv inside the scale.
shift :: Int -> Sequence Prim -> Sequence Prim
shift i = fmap $ Mode $ index i

-- | Modifies the scale from which notes a drawn.
mode :: (Scale -> Scale) -> [Scale -> Pitch] -> (Sequence Prim)
mode f fs = return $ Mode f (Voicing fs)

-- | Chord voicing, picked from a scale.
chord :: Int -> [(Scale -> Pitch)] -> (Sequence Prim)
chord = mode . step

-- | A line defined in terms of abstract scale steps.
line :: [Scale -> Pitch] -> (Sequence Prim)
line = foldr ((:) . Voicing . return) mempty

-- | An arpeggio is a line viewed relative to some chord.
arpeggio :: Int -> [Scale -> Pitch] -> (Sequence Prim)
arpeggio i fs = fmap (Mode $ step i) $ line fs

-- | Derive a musical event from a primitive in some scale
derive :: Scale -> Sequence Prim -> Sequence (Simultanity Pitch)
derive s = fmap (f s)
  where f s (Mode  i v) = f (i s) v
        f s (Voicing v) = foldr ((:=:) . Sound . (\f -> f s)) Silence v

infixr 4 :<: -- motif.
infixr 3 :+: -- sequntial composition.

-- | A phrase is the smallest complete musical composition you can imagine.
data Phrase c p b = Ctrl [c] (Phrase c p b)
                  | Sig (Signature b) (Phrase c p b)
                  | Phrase c p b :+: Phrase c p b
                  | (Sequence p) :<: (Rhythm b)
                  deriving (Show)

data PhraseControl = BendNext
                   | TieNext
                   | Volume   Rational
                   | Legato
                   | Staccato Beat
                   deriving (Eq)

type CPhrase p b = Phrase PhraseControl p b
type Phrase1 = CPhrase Prim                Beat
type Phrase2 = CPhrase (Simultanity Pitch) Beat

instance Semigroup (Phrase c p b) where
  (<>) = (:+:)

instance Measurable (Phrase c Prim) where
  withSignature = Sig
  measure       = foldr (<>) (rest 0) . map rest
  signature ph  = let (_, _, r) = unPhrase ph in signature r
  unmeasure ph  = let (_, _, r) = unPhrase ph in unmeasure r

-- | Constructing phrases, is just combining the underlying structures.
phrase :: (Num b, Ord b) => ([[c]], Sequence p, Rhythm b) -> Phrase c p b
phrase (c, p, b) =
  Sig (signature b) $ foldr1 (:+:) $ map f $ zip3 c p (unmeasure b)
  where f (c, p, b) = Ctrl c (return p :<: measure [b])

-- | A phrase can always be deconstructed.
unPhrase :: (Num b, Ord b) => Phrase c p b -> ([[c]], Sequence p, Rhythm b)
unPhrase (Ctrl c' ph ) = let (c, p, r) = unPhrase ph in (map (c'<>) c, p, r)
unPhrase (Sig  s  ph ) = let (c, p, r) = unPhrase ph in (c, p, withSignature s r)
unPhrase (ph1 :+: ph2) = unPhrase ph1 <> unPhrase ph2
unPhrase (h   :<: r  ) = (map (const []) h', h', measure r')
  where (h', r') = unzip $ zip h (unmeasure r)

-- | The single note phrase
note :: (Scale -> Pitch) -> Beat -> Phrase c Prim Beat
note f b = pitch f :<: beat b

-- | The silent phrase
rest :: (Num a, Ord a) => a -> Phrase c Prim a
rest b = silence :<: beat b

-- On the chance that these functions can propagate through to the top.
class ControlPitchBeatTrifunctor f where
  lift3 :: (Num b, Ord b, Num b', Ord b') =>
           ([c] -> [c']) -> (Sequence p -> Sequence p') -> (Rhythm b -> Rhythm b') ->
           (f c p b -> f c' p' b')
  liftC :: (Num b, Ord b) => ([c] -> [c']) -> f c p b -> f c' p b
  liftC f = lift3 f id id
  liftH :: (Num b, Ord b) => (Sequence p -> Sequence p') -> f c p b -> f c p' b
  liftH f = lift3 id f id
  liftR :: (Num b, Ord b, Num b', Ord b') =>
           (Rhythm b -> Rhythm b') -> f c p b -> f c p b'
  liftR f = lift3 id id f

instance ControlPitchBeatTrifunctor Phrase where
  lift3 f g h = (\(c, p, b) -> phrase (map f c, g p, h b)) . unPhrase

-- Lift functions on lists to phrases.
liftL :: (Num b, Ord b) => ([Int] -> [Int]) -> CPhrase p b -> CPhrase p b
liftL f ph = phrase (c', p', measure b')
  where (c, p, b) = unPhrase ph
        cpb  = zip3 c p (unmeasure b)
        cpb' = [x | (x, i) <- zip cpb [1..], j <- f [1..(length c)], i == j]
        (c', p', b') = unzip3 cpb'

appLast :: PhraseControl -> Phrase1 -> Phrase1
appLast c ph = Sig s $ ph' <> (liftC ((:)c) ph'')
  where s    = signature ph
        ph'  = liftL init ph
        ph'' = liftL (return . last) ph

tie :: Phrase1 -> Phrase1 -> Phrase1
tie ph1 ph2 = appLast TieNext ph1 <> ph2

bendInto :: Phrase1 -> Phrase1 -> Phrase1
bendInto ph1 ph2 = appLast BendNext ph1 <> ph2

legato :: Phrase1 -> Phrase1
legato = liftC ((:)Legato)

staccato :: Beat -> Phrase1 -> Phrase1
staccato b = liftC ((:)(Staccato b))

volume :: Rational -> Phrase1 -> Phrase1
volume v = liftC ((:)(Volume v))

data VoiceState =
  VS { phrases :: [Phrase1]
     , cursor  :: Beat
     , scale   :: Scale
     }

emptyVS :: Scale -> VoiceState
emptyVS s =
  VS { phrases    = []
     , cursor     = 0
     , scale      = s
     }

type Voice = State (VoiceState)

instance Semigroup (Voice a) where
  (<>) = (>>)

runVoice :: (Voice a) -> Scale -> (a, [Phrase2], Beat)
runVoice v s = (a, map (liftH (derive s)) $ phrases vs, cursor vs)
  where (a, vs) = runState v (emptyVS s)

getScale :: Voice Scale
getScale = scale <$> get

putScale :: Scale -> Voice ()
putScale m = get >>= \s -> put $ s {scale = m}

getPhrases :: Voice [Phrase1]
getPhrases = phrases <$> get

putPhrases :: [Phrase1] -> Voice ()
putPhrases phs = get >>= \s -> put $ s {phrases = phs}

singleV :: Phrase1 -> Voice ()
singleV ph = do
  b   <- getTime
  phs <- getPhrases
  s   <- getScale
  putPhrases ((rest b <> liftH (fmap $ Mode (const s)) ph) : phs)
  putTime $ b + duration ph

inKey :: Phrase1 -> Scale -> Voice ()
inKey ph s = do
  s' <- getScale
  putScale s
  singleV ph
  putScale s'

applyMode :: (Scale -> Scale) -> Voice ()
applyMode f = do
  s <- getScale
  putScale $ f s

moreV :: [Phrase1] -> Voice ()
moreV [        ] = return ()
moreV (ph : phs) = do
  b   <- getTime
  singleV ph
  b'  <- getTime
  putTime b
  moreV phs
  b'' <- getTime
  putTime $ max b' b''

getTime :: Voice Beat
getTime = cursor <$> get

putTime :: Beat -> Voice ()
putTime b = do s <- get
               put $ s {cursor = b}

class Player p where
  perform :: p i t -> i -> Phrase2 -> t

class Composition c where
  title  :: c -> String
  tempo  :: c -> Int
  voices :: Player p => c -> Map.Map String ([Phrase2], p i t)
