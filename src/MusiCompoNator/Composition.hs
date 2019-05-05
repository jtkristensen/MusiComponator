{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where

import MusiCompoNator.Core
import Control.Monad.RWS

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

instance Semigroup (Phrase c p b) where
  (<>) = (:+:)

instance Mesurable (Phrase c Prim) where
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

-- -- What about percussion ? (put it in player).

type Voice a c p b = RWS Scale [Phrase c p b] Rational a

runVoice :: (Num b, Ord b) => Voice a c p b ->  Scale -> [c] -> ([Phrase c p b], a)
runVoice v  s  cs = (map (liftC (cs++)) w, a)
  where (a, _, w) = runRWS v s (0 :: Rational)

inKey :: Voice a c p b -> Scale -> Voice a c p b
inKey = flip $ local . const

goto :: Rational -> Voice () c p b
goto = put

getTime :: Voice Rational c p b
getTime = get

more :: [Phrase c Prim Beat] -> Voice () c Prim Beat
more [        ] = return ()
more (ph : phs) =
  do now <- getTime
     tell [rest now <> ph]
     goto now
     more phs
     end <- getTime
     goto $ max end $ now + duration ph

instance Semigroup (Voice a c p b) where
  v1 <> v2 = do now <- getTime
                a <- v1
                x <- getTime
                goto now
                v2
                y <- getTime
                goto $ max x y
                return a

data Player a voice target =
  Player { name    :: String
         , perform :: (voice, a) -> Maybe target
         , costom  :: a
         }

class Composition c where
  add    :: String -> Voice a d p b -> c v t -> Maybe (c v t)
  create :: String -> Player a v t  -> c v t -> Maybe (c v t)
  remove :: String -> c v t -> c v t
  alter  :: String -> ([v] -> [v]) -> c v t -> c v t
  render :: c v t  -> Maybe t
