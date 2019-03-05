
module MusiCompoNator.Core where

-- * Abstract purely harmonic datastructures.

-- | Inspired from "an algebra of music".
infixr 5 :=:
infixr 4 :+:
infixr 3 :.:

-- We distinguish between musical events that are truely parallel
-- (have same duration, and

data Simultanity pitch =
    Silence
  | Sound       pitch
  | Simultanity pitch :=: Simultanity pitch -- truely parallel.

data Sequence pitch =
    Empty
  | Simultanity pitch :+: Sequence pitch    -- truely sequential.
  | Sequence    pitch :.: Sequence pitch    -- composed sequences.

instance Functor Simultanity where
  fmap _ Silence   = Silence
  fmap f (Sound a) = Sound $ f a
  fmap f (a :=: b) = fmap f a :=: fmap f b

instance Functor Sequence where
  fmap _ Empty           = Empty
  fmap f (s :+: equence) = fmap f s :+: fmap f equence
  fmap f (s :.: equence) = fmap f s :.: fmap f equence

-- * Inhabitants for the parameter a

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

index :: Int -> Scale -> Scale
index = step . (+1)

-- * Abstract purely rhythmical datastructures.

type Beat  = Rational
type Meter = (Int, Int)
data Signature =
    Times Int Meter
  | Shift Signature Signature

infixr 3 :|:

data Rhythm beat =
    Measure Meter [beat]
  | Repeat  (Rhythm beat)
  | (Rhythm beat) :|: (Rhythm beat)

instance Functor Rhythm where
  fmap f (Measure m bs) = Measure m $ map f bs
  fmap f (Repeat  r)    = Repeat $ fmap f r
  fmap f (r1 :|: r2)    = fmap f r1 :|: fmap f r2

instance Show Signature where
  show (Times i (m,n)) = show i ++ "x" ++ "(" ++ show m ++ "/" ++ show n ++ ")"
  show (Shift s s') = show s ++ " || " ++ show s'
