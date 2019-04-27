{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where

import MusiCompoNator.Core

-- * Harmonic construction.

-- | Everything is constructed from primitive abstractions on scales.
data Prim =
    Voicing [(Scale -> Pitch)]
  | Mode     (Scale -> Scale) Prim

-- | Transposing a primitive harmonic structure.
transpose :: Pitch -> Prim -> Prim
transpose p = Mode $ fmap (+p)

-- | Shift a primitiv inside the scale.
shift :: Int -> Prim -> Prim
shift i = Mode $ index i

-- | Single pich drawn from some scale.
pitch :: Int -> (Sequence Prim)
pitch i = return (Voicing . return $ root . (step i))

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
derive :: Scale -> Prim -> Simultanity Pitch
derive s (Mode  i v) = derive (i s) v
derive s (Voicing v) = foldr ((:=:) . Sound . (\f -> f s)) Silence v

infixr 4 :<: -- motif
infixr 3 :+: -- sequential phrase compostition (shallow ???) (belongs to voice ???)

type Control = () -- monoid ???

-- A musical phrase, is the composition of a harmonic sequence with a rhythm.
-- Additionally, a phrase may contain information about performance.
data Phrase c h r =
    h :<: r | Phrase c h r :+: Phrase c h r | Control c (Phrase c h r)

-- TODO:
--   lifts        := liftH, liftR, liftC (control).
--   constructors := note, rest, combine.

-- phrase :: Monoid d => Motif h r -> Phrase h r d
-- phrase m = Segment m mempty

-- note :: Monoid d => (Scale -> Pitch) -> Beat -> Phrase Prim Beat d
-- note p b = phrase $ (Voicing [p] :+: Empty) :<: beat b

-- rest :: Monoid d => Beat -> Phrase Prim Beat d
-- rest = phrase . (:<:) (Voicing [] :+: Empty) . beat

-- What about percussion ?

-- Now, what is a voice ?
-- "Scale -> Phrase h r d ?"

-- data Voice a =
--   Single

-- -- A voice is composed of phrases and
-- data Voice a = Voice (Phrase Prim Beat a)

-- instance Functor Voice where
--   fmap f (Voice s ph) = Voice s $ fmap (second (fmap f)) . ph

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

