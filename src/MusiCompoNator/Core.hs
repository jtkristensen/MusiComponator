
module MusiCompoNator.Core where

import Data.Ratio ((%), numerator, denominator)
import Control.Arrow (first)

-- * Abstract purely harmonic datastructures.

-- Inspired from "an algebra of music".
infixr 5 :=:

-- We distinguish between musical events that are truely parallel
-- (have same duration, and

data Simultanity pitch =
    Silence
  | Sound       pitch
  | Simultanity pitch :=: Simultanity pitch -- truely parallel.

-- Inspired from "an algebra of music".
infixr 4 :+:
infixr 3 :.:

data Sequence harmony =
    Empty
  |           harmony :+: Sequence harmony  -- truely sequential.
  | Sequence  harmony :.: Sequence harmony  -- composed sequences.

instance Functor Simultanity where
  fmap _ Silence   = Silence
  fmap f (Sound a) = Sound $ f a
  fmap f (a :=: b) = fmap f a :=: fmap f b

instance Functor Sequence where
  fmap _ Empty           = Empty
  fmap f (s :+: equence) =      f s :+: fmap f equence
  fmap f (s :.: equence) = fmap f s :.: fmap f equence

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

tuplet :: Integer -> Integer -> Beat -> Beat
tuplet n d = (*)(n % d)

dotted, triplet :: Beat -> Beat
dotted  = tuplet 3 2
triplet = tuplet 2 3
-- shuffle goes into rhythm.

-- | Named beats (traditional western).
wn, hn, qn, en, sn :: Beat
wn = 1; hn = 1 % 2; qn = 1 % 4; en = 1 % 8; sn = 1 % 16

infixr 3 :|:
infixr 3 :-:

data Rhythm beat =
    Measure Meter [beat]
  | Repeat  Int (Rhythm beat)
  | (Rhythm beat) :|: (Rhythm beat) -- bar
  | (Rhythm beat) :-: (Rhythm beat) -- tie
    deriving(Show)

instance Functor Rhythm where
  fmap f (Measure m bs) = Measure m $ map f bs
  fmap f (Repeat i r)   = Repeat i $ fmap f r
  fmap f (r1 :|: r2)    = fmap f r1 :|: fmap f r2
  fmap f (r1 :-: r2)    = fmap f r1 :-: fmap f r2

instance Show Signature where
  show (Times i (m,n)) = show i ++ "x" ++ "(" ++ show m ++ "/" ++ show n ++ ")"
  show (Shift s s') = show s ++ " || " ++ show s'

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

-- 'fmap' can change time throw time signatures off.
-- A way of fixing this can be to let 'beat' and 'meater' implement
-- a class, and then let 'rhythm' be parameterized by the class.
-- but this makes the data type confusing i think.
updateSig :: Rhythm Beat -> Rhythm Beat
updateSig (Measure _ bs) = measure bs
updateSig (Repeat  n r ) = Repeat n (updateSig r)
updateSig (r1 :|: r2)    = updateSig r1 :|: updateSig r2
updateSig (r1 :-: r2)    = updateSig r1 :-: updateSig r2

-- We dedicate the name of this transformation to the great J Dilla {^o^}.
-- b a beat, ks proportions, r a rhythm
-- jSigna  :  Subdivision time signature.
-- jTuplet :  Global time scale.
-- jScale  :  Local time scale operation.
-- s       :  Original time signature.
-- s'      :  Globally transformed time signature.
-- l       :  Lenght of ks as an integral.
-- k       :  smallest subdivision (correct ?)
-- (n, d)  :  The (numerator , denominator) of m.
-- TODO -  1) Prove that "signature jDilla m ks r = signature r"
--            for all m > 0 and ks nonempty.
--         2) Check for errors (swing, quintuplets, ..)
--         3) Prove correctness (in what sense?).
--         4) Write better documentation {^_^}.
jDilla :: Meter -> [Integer] -> (Rhythm Beat -> Rhythm Beat)
jDilla m ks r =
  fromTime s $ fst $ jScale (concat (repeat ks)) $ jTuplet $ fromTime s' r
  where
    (n, d) = m
    k      = numerator $ (foldl lcm d $ map denominator jSigna) % d
    l   = (fromIntegral $ length ks)
    s   = signature r
    s'  = Shift (signature $ beat $ ((meter2beat m) / l)) s'
    jSigna  = map (\k -> k % (sum ks) * (meter2beat m)) ks
    jTuplet = updateSig . (fmap $ tuplet n k)
    jScale (n : ns) (Measure _ bs) = (measure $ map ((*) $ fromIntegral n) bs, ns)
    jScale ns (Repeat 0 _)   = (measure [], ns)
    jScale ns (Repeat l r)   =
      let (rb, ns') = jScale ns r in  first (rb:|:) $ jScale ns' (Repeat (l - 1) r)
    jScale ns (r1 :|: r2)    =
      let (rb, ns') = jScale ns r1 in  first (rb:|:) $ jScale ns' r2
    jScale ns (r1 :-: r2)    =
      let (rb, ns') = jScale ns r1 in  first (rb:-:) $ jScale ns' r2
    jScale _ _ = error "does not happen by construction."

-- Common shuffles
shuffle8, shuffle16 :: Rhythm Beat -> Rhythm Beat
shuffle8  = jDilla (2,  8) [2, 1]
shuffle16 = jDilla (2, 16) [2, 1]

-- | Some named infinite rhythms.
wns, hns, qns, ens, sns :: Rhythm Beat
wns = beat wn :|: wns
hns = beat hn :|: hns
qns = beat qn :|: qns
ens = beat en :|: ens
sns = beat sn :|: sns
