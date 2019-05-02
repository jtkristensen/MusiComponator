{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where

import MusiCompoNator.Core
-- import Data.Bifunctor

-- * Harmonic construction.

-- | Everything is constructed from primitive abstractions on scales.
data Prim =
    Voicing [(Scale -> Pitch)]
  | Mode     (Scale -> Scale) Prim

-- | Single concrete pitch (not recommended).
absPitch :: Rational -> (Sequence Prim)
absPitch q = return (Voicing . return $ const q)

-- | Single pitch drawn from some scale.
relPitch :: (Scale -> Pitch) -> (Sequence Prim)
relPitch i = return (Voicing . return $ i)

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
note f b = relPitch f :<: beat b

-- | The silent phrase
rest :: (Num a, Ord a) => a -> Phrase c Prim a
rest = (:<:) silence . beat

-- | Lifts a control modification to phrase-level.
liftC :: (Num b, Ord b) => (c -> c') -> Phrase c p b -> Phrase c' p b
liftC f ph = let (css, p, b) = unPhrase ph in phrase (map (map f) css, p, b)

-- | Lifts a function on melodys to phrase-level.
liftH :: (Num b, Ord b) => (Sequence p -> Sequence p') -> Phrase c p b -> Phrase c p' b
liftH f ph = let (c, p, b) = unPhrase ph in phrase (c, f p, b)

-- | Lifts a function on rhythms to phrase-level.
liftR :: (Num b, Ord b) => (Rhythm b -> Rhythm b) -> Phrase c p b -> Phrase c p b
liftR f ph = let (c, p, b) = unPhrase ph in phrase (c, p, f b)

-- What about percussion ?

-- Control.Monad.Writer
-- data Voice ph = Polyphonic [ph]

-- data Player a b = Player { name    :: String
--                          , perform :: Vocie a -> b
--                          }

-- data Player a b =
--   Player { name    :: String
--          , perform :: Voice a -> b
--          }

-- data Composition a b =
--   Composition { title  ::  String
--               , tempo  ::  Int
--               , key    ::  Scale
--               , voices :: [(Player a b, Voice a)]
--               }

-- instance Semigroup (Composition a b) where
--   c1 <> c2 = c1 { voices = voices c1 <> voices c2 }

-- new :: String -> Composition a b
-- new name =
--   Composition { title  = name
--               , tempo  = 120
--               , key    = (ionian c)
--               , voices = []
--               }

-- setTempo :: Int -> Composition a b -> Composition a b
-- setTempo meter c = c { tempo = meter }

-- setKey :: Scale -> Composition a b -> Composition a b
-- setKey k c = c { key = k }

-- on :: Voice a -> Player a b -> Composition a b
-- on v p = (new "Voice") { voices = [(p, v)]}
