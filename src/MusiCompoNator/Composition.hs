{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where

import MusiCompoNator.Core
import Data.Bifunctor

-- * Harmonic construction.

-- | Everything is constructed from primitive abstractions on scales.
data Prim =
    Voicing [(Scale -> Pitch)]
  | Mode     (Scale -> Scale) Prim

-- | Transposing a primitive harmonic structure.
transpose :: Pitch -> Prim -> Prim
transpose p = Mode $ map (+p)

-- | Shift a primitiv inside the scale.
shift :: Int -> Prim -> Prim
shift i = Mode $ index i

-- | Single pich drawn from some scale.
pitch :: Int -> (Sequence Prim)
pitch i = (Voicing . return $ root . (step i)) :+: Empty

-- | Chord voicing, picked from a scale.
chord :: Int -> [(Scale -> Pitch)] -> (Sequence Prim)
chord i fs = Mode (step i) (Voicing fs) :+: Empty

-- | A line defined in terms of abstract scale steps.
line :: [Scale -> Pitch] -> (Sequence Prim)
line = foldr (\f xs -> Voicing [f] :+: xs) Empty

-- | An arpeggio is a line viewed relative to some chord.
arpeggio :: Int -> [Scale -> Pitch] -> (Sequence Prim)
arpeggio i fs = fmap (Mode $ step i) $ line fs

-- | Derive a musical event from a primitive in some scale
derive :: Scale -> Prim -> Simultanity Pitch
derive s (Mode  i v) = derive (i s) v
derive s (Voicing v) = foldr (\x xs -> Sound (x s) :=: xs) Silence v

infix 4 :<:

-- | A motif, is a sequence of primitives played with a rhythm.
data Motif h r = (Sequence h) :<: (Rhythm r)

instance Bifunctor Motif where
  bimap f g (h :<: r) = fmap f h :<: fmap g r

-- * The simplest of motifs.

note :: (Scale -> Pitch) -> Beat -> Motif Prim Beat
note p b = (Voicing [p] :+: Empty) :<: (beat b)

rest :: Beat -> Motif Prim Beat
rest = (:<:) (Voicing [] :+: Empty) . beat

-- * Constructing phrases from motifs.

-- Where ??
-- infixr 3 :..:
-- infixr 2 :++:
-- t m d :..: t m d
-- t m d :++: t m d

-- A phrase is a special motif, that has a concrete musical meaning,
-- and which parameterizes rhythms with dynamic properties that we
-- shall refer to as 'phrasing'.
data Phrase m d = Phrase m d

instance Bifunctor Phrase where
  bimap f g (Phrase m d) = Phrase (f m) (g d)

-- data Voice =
--     Voice (Scale -> Motif (Simultanity Pitch) (Beat, Phrasing))
  -- | Voice :.: Voice -- polyphonic composition. ??

-- What about percussion ?

-- Now, what is a voice ?
-- "Scale -> Phrase h r d ?"


-- data Voice a =
--   Single

-- -- -- A voice is composed of phrases and
-- -- data Voice a = Voice (Phrase Prim Beat a)

-- -- instance Functor Voice where
-- --   fmap f (Voice s ph) = Voice s $ fmap (second (fmap f)) . ph

-- -- data Player a b =
-- --   Player { name    :: String
-- --          , perform :: Voice a -> b
-- --          }

-- -- data Composition a b =
-- --   Composition { title  ::  String
-- --               , tempo  ::  Int
-- --               , key    ::  Scale
-- --               , voices :: [(Player a b, Voice a)]
-- --               }

-- -- instance Semigroup (Composition a b) where
-- --   c1 <> c2 = c1 { voices = voices c1 <> voices c2 }

-- -- new :: String -> Composition a b
-- -- new name =
-- --   Composition { title  = name
-- --               , tempo  = 120
-- --               , key    = (ionian c)
-- --               , voices = []
-- --               }

-- -- setTempo :: Int -> Composition a b -> Composition a b
-- -- setTempo meter c = c { tempo = meter }

-- -- setKey :: Scale -> Composition a b -> Composition a b
-- -- setKey k c = c { key = k }

-- -- on :: Voice a -> Player a b -> Composition a b
-- -- on v p = (new "Voice") { voices = [(p, v)]}

