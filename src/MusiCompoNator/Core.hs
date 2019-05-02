{-# LANGUAGE FlexibleInstances #-}

module MusiCompoNator.Core where

import Data.Ratio ((%))

-- * Purely harmonic abstractions.

-- Inspired from "an algebra of music", but concerns itself with
-- parallel composition only.
infixr 5 :=:

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

type Sequence harmony = [harmony]
type Pitch            = Rational
type Scale            = [Pitch]

-- | Named Pitches.
c, d, e, f, g, a, b :: Pitch
c = 0; d = 2; e = 4; f = 5; g = 7; a = 9; b = 11

-- | Scale movements.
up, down, sharp, flat :: Pitch -> Pitch
up    = (+) 12
down  = flip (-) 12
sharp = (+) 1
flat  = flip (-) 1

root :: Scale -> Pitch
root = head

-- | A simple scale inversion.
invertr, invertl :: Scale -> Scale
invertr scale = tail scale ++ [up (head scale)]
invertl scale = down (last scale) : init scale

-- | Scale inversion at step n.
step :: Int -> Scale -> Scale
step 1 s = s
step n s = if   n < 0
           then step (n + 1) (invertl s)
           else step (n - 1) (invertr s)

-- | Index into a scale (more mathematical version of step).
index :: Int -> Scale -> Scale
index = step . (+1)

-- | Named scale step abstractions.
i, ii, iii, iv, v, vi, vii, viii, ix, x, xi, xii, xiii :: Scale -> Pitch
i    = root . step  1;   ii = root . step  2;  iii = root . step  3
iv   = root . step  4;    v = root . step  5;   vi = root . step  6
vii  = root . step  7; viii = root . step  8;   ix = root . step  9
x    = root . step 10;   xi = root . step 11;  xii = root . step 12
xiii = root . step 13

-- | A diatonic mode constructors.
ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Pitch -> Scale
ionian     q = map ((+) q) [0, 2, 4, 5, 7, 9, 11]
dorian     q = step 2 $ ionian (q -  2)
phrygian   q = step 3 $ ionian (q -  4)
lydian     q = step 4 $ ionian (q -  5)
mixolydian q = step 5 $ ionian (q -  7)
aeolian    q = step 6 $ ionian (q -  9)
locrian    q = step 7 $ ionian (q - 11)

-- TODO : primary scales from chords.

-- * Purely rhythmical abstractions.

type Beat        = Rational
data Signature a = Times Int a | Shift (Signature a) (Signature a)

instance Show a => Show (Signature a) where
  show (Times i  m) = show i ++ "x" ++ "(" ++ show m ++ ")"
  show (Shift s s') = show s ++ " || " ++ show s'

tuplet :: Integer -> Integer -> Rhythm Beat -> Rhythm Beat
tuplet n d = fmap $ (*)(n % d)

dotted, triplet :: Rhythm Beat -> Rhythm Beat
dotted  = tuplet 3 2
triplet = tuplet 2 3
-- shuffle goes into rhythm.

-- | Named beats (traditional western).
wn, hn, qn, en, sn :: Beat
wn = 1 % 1; hn = 1 %  2; qn = 1 % 4; en = 1 % 8; sn = 1 % 16;

-- Measures can be separated in two ways.
infixr 3 :|: -- bar
infixr 3 :-: -- tie

data Rhythm beat =
    Measure [beat]
  | Repeat  Int (Rhythm beat)
  | (Rhythm beat) :|: (Rhythm beat)
  | (Rhythm beat) :-: (Rhythm beat)
    deriving(Show)

instance Functor Rhythm where
  fmap f (Measure bs) = Measure $ map f bs
  fmap f (Repeat i r) = Repeat i $ fmap f r
  fmap f (r1 :|: r2)  = fmap f r1 :|: fmap f r2
  fmap f (r1 :-: r2)  = fmap f r1 :-: fmap f r2

instance Semigroup (Rhythm b) where
  (<>) = (:|:)

-- * Simple rhythms

beat :: (Num b, Ord b) => b -> Rhythm b
beat b = measure [b]

class Mesurable m where
  withSignature :: (Num a, Ord a) => (Signature a) -> (m a) -> (m a)
  signature     :: (Num a, Ord a) => (m a) -> (Signature a)
  measure       :: (Num a, Ord a) => [a] -> m a
  unmeasure     :: (Num a, Ord a) => m a -> [a]

instance Mesurable Rhythm where
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
      meters (Repeat      0 _ ) = []
      meters (Repeat      n r ) = meters r ++ meters (Repeat (n - 1) r)
      meters (r1 :|: r2)        = meters r1 ++ meters r2
      meters (r1 :-: r2)        = meters r1 ++ meters r2
      collect [s] = s
      collect (Times n k : Times m k' : s) =
        if   k == k'
        then collect (Times (n + m) k : s)
        else Shift (Times n k) $ collect (Times m k' : s)
      collect _ = error "impossible (by construction)."
  measure = Measure
  unmeasure (Measure bs)   = bs
  unmeasure (Repeat  i rh) = foldr (++) [] (map (const $ unmeasure rh) [1..i])
  unmeasure (r1 :|: r2)    = unmeasure r1 ++ unmeasure r2
  unmeasure (r1 :-: r2)    = unmeasure r1 `tie` unmeasure r2
    where tie []  r2      = r2
          tie [x] (h : t) = (x + h : t)
          tie (x : xs) t  = x : tie xs t

duration :: (Num a, Ord a, Mesurable m) => m a -> a
duration = sum . unmeasure

-- | Some named infinite rhythms.
wns, hns, qns, ens, sns :: Int -> Rhythm Beat
wns n = measure $ take n $ repeat wn
hns n = measure $ take n $ repeat hn
qns n = measure $ take n $ repeat qn
ens n = measure $ take n $ repeat en
sns n = measure $ take n $ repeat sn

