{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MusiCompoNator.Composition where

import MusiCompoNator.Core
import Data.Bifunctor

-- import Control.Monad
-- import Data.Ratio

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
pitch :: Int -> Prim
pitch i = Voicing . return $ root . (step i)

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

infixr 4 :+
infix  3 :<

-- | A motif, is a sequence of primitives played with a rhythm.
data Motif h r =
    (Sequence h) :< (Rhythm r)
  | (Motif h r)  :+ (Motif h r)

appH :: (Sequence a -> Sequence b) -> Motif a r -> Motif b r
appH f (h  :< r ) = f h :< r
appH f (m0 :+ m1) = appH f m0 :+ appH f m1

appR :: (Rhythm   a -> Rhythm   b) -> Motif h a -> Motif h b
appR f (h  :< r ) = h         :< f r
appR f (m0 :+ m1) = appR f m0 :+ appR f m1

instance Bifunctor Motif where
  bimap f g = (appH $ fmap f) . (appR $ fmap g)

class HRF f where -- functor on harmony and rhythm

-- * The simplest of motifs.

note :: (Scale -> Pitch) -> Beat -> Motif Prim Beat
note p b = (Voicing [p] :+: Empty) :< (beat b)

rest :: Beat -> Motif Prim Beat
rest = (:<) (Voicing [] :+: Empty) . beat

-- -- * Constructing phrases from motifs.

-- infixr 3 :.:

-- A phrase adds a dymanic parts which we shall refer to as 'phrasing'.
-- data Phrase hd rd h r =
--     Phrase (Motif (h, hd) (r, rd))
--   | Phrase hd rd h r :.: Phrase hd rd h r

-- instance HRF (Phrase a b) where
--   liftH f (Phrase m) = Phrase $ liftH . first . f $ m

-- -- For the sake of lifting to a Phrase to HR.
-- -- Is there a more elegant way of doing this?
-- data HRPhrase d h r = HRP (Phrase h r d)
-- instance HR (HRPhrase d) where
--   hmap f (HRP (Phrase m)) = HRP $ Phrase $ hmap (second f) m
--   rmap f (HRP (Phrase m)) = HRP $ Phrase $ rmap f m

-- (+^) :: Phrase h r a -> Phrase h r a -> Phrase h r a
-- (+^) (Phrase p) (Phrase q) = Phrase $ p :+ q

-- opensWith, endsWith ::  Phrase h r a -> (a -> a) -> Phrase h r a

-- opensWith (Phrase p) f = Phrase $ (appH . appHead . first) f p
--   where appHead _ Empty = Empty
--         appHead f (s  :+: eq) = f s :+: eq
--         appHead f (s1 :.: s2) = appHead f s1 :.: appHead f s2

-- endsWith (Phrase p) f = Phrase $ (appH . appTail . first) f p
--   where appTail _ Empty = Empty
--         appTail f (s  :+: Empty) = f s :+: Empty
--         appTail f (s1 :+: s2)    = s1 :+: appTail f s2
--         appTail f (s1 :.: s2)    = appTail f s1 :.: appTail f s2

-- connect :: (a -> a) -> (a -> a) -> Phrase h r a -> Phrase h r a -> Phrase h r a
-- connect f g p1 p2 = (p1 `endsWith` f) +^ (p2 `opensWith` g)

-- dub :: Monoid d => (Sequence h) -> Phrase h r d -> Phrase h r d
-- dub s (Phrase m) = Phrase $ appH (\s' -> s' :.: fmap ((,) mempty) s) m

-- -- What about percussion ?

-- -- Now, what is a voice ?
-- -- "Scale -> Phrase h r d ?"

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

