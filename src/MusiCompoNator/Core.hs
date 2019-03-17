
module MusiCompoNator.Core where

import Data.Ratio ((%), numerator, denominator)

-- * Abstract purely harmonic datastructures.

-- Inspired from "an algebra of music".
infixr 5 :=:
infixr 4 :+:

data Simultanity pitch =
    Silence
  | Sound       pitch
  | Simultanity pitch :=: Simultanity pitch -- truely parallel.
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

data Sequence harmony =
  Empty | harmony :+: Sequence harmony    -- truely sequential.

instance Functor Sequence where
  fmap _ Empty           = Empty
  fmap f (s :+: equence) = f s :+: fmap f equence

instance Semigroup (Sequence harmony) where
  Empty         <> s2 = s2
  (harm :+: s1) <> s2 = harm :+: (s1 <> s2)

instance Monoid (Sequence harmony) where
  mempty  = Empty
  mappend = (<>)

instance Foldable Sequence where
  foldr _ e (Empty  ) = e
  foldr f e (p :+: s) = f p $ foldr f e s

-- * Inhabitants for the parameter 'pitch'.

-- | From second version of Fb.
type Pitch = Rational

-- | Movements.
up, down, sharp, flat :: Pitch -> Pitch
up    = (+) 12
down  = flip (-) 12
sharp = (+) 1
flat  = flip (-) 1

-- | A scale spelled out in a single octave.
type Scale = [Pitch]

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

-- * Abstract purely rhythmical datastructures.

type Beat  = Rational
type Meter = (Integer, Integer)
data Signature =
    Times Int Meter
  | Shift Signature Signature

instance Show Signature where
  show (Times i (m,n)) = show i ++ "x" ++ "(" ++ show m ++ "/" ++ show n ++ ")"
  show (Shift s s') = show s ++ " || " ++ show s'

tuplet :: Integer -> Integer -> Rhythm Beat -> Rhythm Beat
tuplet n d = fmap $ (*)(n % d)

dotted, triplet :: Rhythm Beat -> Rhythm Beat
dotted  = tuplet 3 2
triplet = tuplet 2 3
-- shuffle goes into rhythm.

-- | Named beats (traditional western).
wn, hn, qn, en, sn :: Rhythm Beat
wn = beat $ 1 % 1; hn = beat $ 1 %  2; qn = beat $ 1 % 4
en = beat $ 1 % 8; sn = beat $ 1 % 16;

-- Measures can be separated in two ways.
infixr 3 :|: -- bar
infixr 3 :-: -- tie

data Rhythm beat =
    Measure Meter [beat]
  | Repeat  Int (Rhythm beat)
  | (Rhythm beat) :|: (Rhythm beat)
  | (Rhythm beat) :-: (Rhythm beat)
    deriving(Show)

instance Functor Rhythm where
  fmap f (Measure m bs) = Measure m $ map f bs
  fmap f (Repeat i r)   = Repeat i $ fmap f r
  fmap f (r1 :|: r2)    = fmap f r1 :|: fmap f r2
  fmap f (r1 :-: r2)    = fmap f r1 :-: fmap f r2

instance Semigroup (Rhythm a) where
  (<>) = (:|:)

-- * Simple rhythms

beat :: Beat -> Rhythm Beat
beat b = measure [b]

beat2meter :: Beat -> Meter
beat2meter b = (numerator b, denominator b)

meter2beat :: Meter -> Beat
meter2beat m = fst m % snd m

measure :: [Beat] -> Rhythm Beat
measure bs = Measure (beat2meter (foldl (+) 0 bs)) bs

unmeasure :: Rhythm Beat -> [Beat]
unmeasure (Measure _ bs) = bs
unmeasure (Repeat  i rh) = foldr (++) [] (map (const $ unmeasure rh) [1..i])
unmeasure (r1 :|: r2)    = unmeasure r1 ++ unmeasure r2
unmeasure (r1 :-: r2)    = unmeasure r1 `tie` unmeasure r2
  where tie []  r2      = r2
        tie [x] (h : t) = (x + h : t)
        tie (x : xs) t  = x : tie xs t

fromTime :: Signature -> Rhythm Beat -> Rhythm Beat
fromTime s r = aquire [] (meters s) (unmeasure r)
  where
    meters (Times n   m) = map (const $ fst m % snd m) [1..n]
    meters (Shift s0 s1) = meters s0 ++ meters s1
    aquire m _        [      ] = measure (reverse m)
    aquire m [      ] _        = measure (reverse m)
    aquire m (q : qs) (b : bs) =
      case q `compare` b of
        EQ -> measure (reverse $ b : m) :|: aquire [     ] qs           bs
        LT -> measure (reverse $ q : m) :-: aquire [     ] qs  (b - q : bs)
        GT ->                               aquire (b : m) (q - b : qs) bs

signature :: Rhythm Beat -> Signature
signature = collect . meters
  where
    meters (Measure (0, 1) _) = []
    meters (Measure     m _ ) = [Times 1 m]
    meters (Repeat      0 _ ) = []
    meters (Repeat      n r ) = meters r ++ meters (Repeat (n - 1) r)
    meters (r1 :|: r2)        = meters r1 ++ meters r2
    meters (r1 :-: r2)        = meters r1 ++ meters r2
    collect [s] = s
    collect (Times n k : Times m k' : s) =
      if   k == k'
      then collect (Times (n + m) k : s)
      else Shift (Times n k) $ collect (Times m k' : s)
    collect _ = error "impossible by construction."

duration :: Rhythm Beat -> Rational
duration = sum . unmeasure

-- | Some named infinite rhythms.
wns, hns, qns, ens, sns :: Int -> Rhythm Beat
wns n = measure $ take n $ foldr (++) [] $ repeat $ unmeasure wn
hns n = measure $ take n $ foldr (++) [] $ repeat $ unmeasure hn
qns n = measure $ take n $ foldr (++) [] $ repeat $ unmeasure qn
ens n = measure $ take n $ foldr (++) [] $ repeat $ unmeasure en
sns n = measure $ take n $ foldr (++) [] $ repeat $ unmeasure sn

-- TODO:
-- In the future, I would like liftH and liftR to live in this module.
-- class Harmonic h where
--   liftH :: (a -> b) -> h a -> h b
-- instance Harmonic Simultanity where
--   liftH = fmap
-- instance Harmonic Sequence where
--   liftH = fmap
-- class Rhythmic r where
--   liftR :: (a -> b) -> r a -> r b
-- instance Rhythmic Rhythm where
--   liftR = fmap
