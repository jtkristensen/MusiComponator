
module MusiCompoNator.Composition where

import MusiCompoNator.Core
import Control.Arrow (second)
import Data.Ratio

-- * Harmonic construction.

-- | Everything is constructed from abstractions on scales.
data Primitive = Pitch (Scale -> Pitch)
               | Chord (Scale -> Scale) [Scale -> Pitch]

-- | Single pich drawn from some scale.
pitch :: Int -> Primitive
pitch i = Pitch $ root . (step i)

-- | Chord voicing, picked from a scale.
voicing :: Int -> [Scale -> Pitch] -> Primitive
voicing i = Chord (step i)

-- | Named pitch.
c, d, e, f, g, a, b :: Pitch
c = 0; d = 2; e = 4; f = 5; g = 7; a = 9; b = 11;

-- | The root note at step n. (should probably not be exported).
get :: Int -> Scale -> Pitch
get n = root . step n

-- | Named step abstaction.
i, ii, iii, iv, v, vi, vii, viii, ix, x, xi, xii, xiii :: Scale -> Pitch
i  = get  1;  ii = get  2;  iii = get  3; iv = get 4; v = get  5
vi = get  6; vii = get  7; viii = get  8; ix = get 9; x = get 10
xi = get 11; xii = get 12; xiii = get 13

-- | A diatonic mode (relative to the c-major scale).
ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Pitch -> Scale
ionian     q = map ((+) q) [c, d, e, f, g, a, b]
dorian     q = step 2 $ ionian (q - d)
phrygian   q = step 3 $ ionian (q - e)
lydian     q = step 4 $ ionian (q - f)
mixolydian q = step 5 $ ionian (q - g)
aeolian    q = step 6 $ ionian (q - a)
locrian    q = step 7 $ ionian (q - b)

-- | Derive a musical event from a primitive in some scale
derive :: Scale -> Primitive -> Simultanity Pitch
derive s (Pitch f)    = Sound (f s)
derive s (Chord i fs) = foldr (:=:) Silence $ map (derive (i s) . Pitch) fs

-- | Inspired from "an algebra of music".
infixr 4 :+:

-- | A motif, is a sequence of primitives played with a rhythm.
data Motif a = Motif [a] Rhythm | (Motif a) :+: (Motif a)

instance Functor Motif where
  fmap f (Motif a b) = Motif (fmap f a) b
  fmap f (m1 :+: m2) = fmap f m1 :+: fmap f m2

-- * Basic rhythmic transformations

-- | Generalized tuplet (borrowed from Hudak).
tuplet :: Integer -> Integer -> (Motif a) -> (Motif a)
tuplet n m (Motif h r) = Motif h (map ((*) (n % m)) r)
tuplet n m (m1 :+: m2) = tuplet n m m1 :+: tuplet n m m2

-- | Simple rhythmic transformations
dotted, triplet :: (Motif a) -> (Motif a)
dotted  = tuplet 3 2
triplet = tuplet 2 3
-- no shuffle (yet).

-- | Named beats (traditional western).
wn, hn, qn, en, sn :: Beat
wn = 1; hn = 1 % 2; qn = 1 % 4; en = 1 % 8; sn = 1 % 16

-- * The simplest of motifs.

note :: (Scale -> Pitch) -> Beat -> Motif Primitive
note p = Motif [Pitch p] . return

-- | The silent motif
rest :: Beat -> Motif Primitive
rest = chord 1 []

-- | The i'th chord in some scale, voiced as fs.
chord :: Int -> [Scale -> Pitch] -> Beat -> Motif Primitive
chord i fs = Motif [voicing i fs] . return

arpeggio :: Int -> [Scale -> Pitch] -> [Primitive]
arpeggio i fs = foldr (\x xs -> voicing i [x] : xs) [] fs

-- | An abstract phrase `with` a rhythm
with :: [Primitive] -> Rhythm -> Motif Primitive
with = Motif

-- * Constructing phrases from motifs.

type Phrase a = Scale -> Motif (Simultanity Pitch, [a])

before :: Phrase a -> Phrase a -> Phrase a
before ph1 ph2 = \s -> ph1 s :+: ph2 s

-- | We can simply play the motif.
play :: Motif Primitive -> Phrase a
play motif = \s -> fmap (\h -> (derive s h, [])) motif

-- | We can add 'effects' (for instance midi controller events).
effect :: a -> Phrase a -> Phrase a
effect a ph = \s -> fmap (second (a:)) $ ph s

-- | We can change mode relative to the current tonality.
mode :: (Scale -> Scale) -> Phrase a -> Phrase a
mode m ph = \s -> ph (m s)

-- | Now transposition is just a mode translation.
transpose :: Pitch -> Phrase a -> Phrase a
transpose q = mode (map (+q))

data Voice a =  Voice Scale (Phrase a)

instance Functor Voice where
  fmap f (Voice s ph) = Voice s $ fmap (second (fmap f)) . ph

data Player a b =
  Player { name    :: String
         , perform :: Voice a -> b
         }

data Composition a b =
  Composition { title  ::  String
              , tempo  ::  Int
              , key    ::  Scale
              , voices :: [(Player a b, Voice a)]
              }

instance Semigroup (Composition a b) where
  c1 <> c2 = c1 { voices = voices c1 <> voices c2 }

new :: String -> Composition a b
new name =
  Composition { title  = name
              , tempo  = 120
              , key    = (ionian c)
              , voices = []
              }

setTempo :: Int -> Composition a b -> Composition a b
setTempo meter c = c { tempo = meter }

setKey :: Scale -> Composition a b -> Composition a b
setKey k c = c { key = k }

on :: Voice a -> Player a b -> Composition a b
on v p = (new "Voice") { voices = [(p, v)]}

