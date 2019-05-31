{-|
  Module      : MC.Lib.Rhythm.Western
  Description : A library that describes common rhythmic transformations in popular music (such as hip hop, RnB and Jazz).
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Lib.Rhythm.Western where

import MC.Core
import Data.Ratio (numerator, denominator, (%))
import Debug.Trace

-- | In Dilla inspired drum grooves, we often need a more fine grained shuffle
--   variant. This n-tuplet shuffle is formally refered to as "The Dilla Feel",
--   and this function obtains the variant by trading a weigted discrete
--   distribution on beats for another distribution of the same lenght.
--   The distributions are represented as lists of integers and must be non-empty,
dillaFeel :: Beat -> [Integer] -> [Integer] -> Rhythm1 -> Rhythm1
dillaFeel b dIn dOut r = withSignature (signature r) $ fst $ jScale ratios r'
  where
    ins      = sum dIn
    outs     = sum dOut
    ratesIn  = map (\n -> (n % ins ) * b) dIn
    ratesOut = map (\n -> (n % outs) * b) dOut
    ratios   = zipWith rate ratesOut ratesIn
    rate x y = (numerator x * denominator y) % (numerator y * denominator x)
    m          = (duration r / b) * ((sum dIn) % 1)
    (n, d)     = (numerator m, denominator m)
    r'         = withSignature jSigna r
    jSigna     =
      foldr1 Shift $ take (fromIntegral (n `div` d) + 1) $
      concat $ repeat $ map (Times 1) $ map (\k -> (k % 1) * (1 % ins) * b) $
      map fromIntegral dIn
    jScale (n : ns) (Measure bs)  = (measure $ map ((*)n) bs, ns ++ [n])
    jScale      ns  (r1  :|:  r2) = (r1' :|: r2', ns')
      where (r1', ns'') = jScale ns   r1
            (r2', ns' ) = jScale ns'' r2
    jScale      ns  (r1  :-:  r2) = (r1' :-: r2', ns')
      where (r1', ns'') = jScale ns   r1
            (r2', ns' ) = jScale ns'' r2
    jScale _        _             = trace "empty" (beat 0, mempty)

shuffle8, shuffle16, swing8, swing16 :: Rhythm1 -> Rhythm1
-- | Ordinary eight note shuffle (1 % 8 + 1 % 8) ~> (1 % 6 + 1 % 12)
shuffle8  = dillaFeel (2 %  8) [1, 1] [2, 1]
-- | Ordinary sixteenth note shuffle (1 % 16 + 1 % 16) ~> (1 % 12 + 1 % 24)
shuffle16 = dillaFeel (2 % 16) [1, 1] [2, 1]
-- | Ordinary eight note swing (1 % 8 + 1 % 8) ~> (3 % 16 + 1 % 16)
swing8    = dillaFeel (2 %  8) [1, 1] [3, 1]
-- | Ordinary sixteenth note swing (1 % 16 + 1 % 16) ~> (3 % 32 + 1 % 32)
swing16   = dillaFeel (2 % 16) [1, 1] [3, 1]

unShuffle8, unShuffle16, unSwing8, unSwing16 :: Rhythm1 -> Rhythm1
-- | Ordinary eight note shuffle (1 % 8 + 1 % 8) ~> (1 % 6 + 1 % 12)
unShuffle8  = dillaFeel (2 %  8) [2, 1] [1, 1]
-- | Ordinary sixteenth note shuffle (1 % 16 + 1 % 16) ~> (1 % 12 + 1 % 24)
unShuffle16 = dillaFeel (2 % 16) [2, 1] [1, 1]
-- | Ordinary eight note swing (1 % 8 + 1 % 8) ~> (3 % 16 + 1 % 16)
unSwing8    = dillaFeel (2 %  8) [3, 1] [1, 1]
-- | Ordinary sixteenth note swing (1 % 16 + 1 % 16) ~> (3 % 32 + 1 % 32)
unSwing16   = dillaFeel (2 % 16) [3, 1] [1, 1]

-- | A named beat durations in western music,
--   whole note, half note, quater note, eight note ...
wn, hn, qn, en, sn, tsn, ssn :: Beat
wn = 1 %  1; hn  = 1 %  2; qn  = 1 %  4; en = 1 % 8
sn = 1 % 16; tsn = 1 % 32; ssn = 1 % 64

-- | Returns a 'Rhythm' consisting of @n@ consequtive 'Beat's
wns, hns, qns, ens, sns, tsns, ssns :: Int -> Rhythm Beat
wns  n = measure $ take n $ repeat  wn
hns  n = measure $ take n $ repeat  hn
qns  n = measure $ take n $ repeat  qn
ens  n = measure $ take n $ repeat  en
sns  n = measure $ take n $ repeat  sn
tsns n = measure $ take n $ repeat tsn
ssns n = measure $ take n $ repeat ssn

-- | A generalized tuplet, inspired by 'An Algebra of Music' by Paul Hudak.
tuplet :: Integer -> Integer -> Rhythm Beat -> Rhythm Beat
tuplet n d = fmap $ (*)(n % d)

-- | A common usages of the generalized tuplet.
dotted, triplet :: Rhythm Beat -> Rhythm Beat
dotted  = tuplet 3 2
triplet = tuplet 2 3
