
module MusiCompoNator.Core where

-- * Basic constructs.

-- | From second version of Fb.
type Pitch = Rational
type Beat  = Rational

-- | Inspired from "an algebra of music".
infixr 4 :=:

-- | Simultaneous music event.
data Simultanity a =
    Silence
  | Sound a
  | (Simultanity a) :=: (Simultanity a)
  deriving (Show)

instance Functor Simultanity where
  fmap _ Silence   = Silence
  fmap f (Sound a) = Sound (f a)
  fmap f (a :=: b) = (fmap f a) :=: (fmap f b)

-- | A simple rhythm
type Rhythm = [Beat]

-- | A scale spelled out in a single octave.
type Scale = [Pitch]

-- * Simple scale arithmetic.

-- | Movements.
up, down, sharp, flat :: Pitch -> Pitch
up    = (+) 12
down  = flip (-) 12
sharp = (+) 1
flat  = flip (-) 1

(<~) :: (a -> b) -> (b -> b) -> (a -> b)
(<~) = flip (.)

-- | The root note.
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

-- | Invert scale n steps.
steps :: Int -> Scale -> Scale
steps = step . (+1)
