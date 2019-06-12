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

shuffle, swing :: Beat -> Rhythm1 -> Rhythm1
-- | Ordinary shuffle (b % 2 + b % 2) ~> triplet (b + b % 2)
shuffle b = dillaFeel b [1, 1] [2, 1]
-- | Ordinary eight note swing (b % 2 + b % 2) ~> dotted (b % 2) + b % 4
swing   b = dillaFeel b [1, 1] [3, 1]

unshuffle, unswing :: Beat -> Rhythm1 -> Rhythm1
-- | unshuffle b (shuffle b r) = shuffle b (unshuffle b r) = id r
unshuffle b = dillaFeel b [2, 1] [1, 1]
-- | unswing b (swing b r) = swing b (unswing b r) = id r
unswing   b = dillaFeel b [3, 1] [1, 1]

-- | A named beat durations in western music,
--   whole note, half note, quater note, eight note ...
wn, hn, qn, en, sn, tsn, ssn :: Rhythm1
wn = beat $ 1 %  1; hn  = beat $ 1 %  2; qn  = beat $ 1 %  4; en = beat $ 1 % 8
sn = beat $ 1 % 16; tsn = beat $ 1 % 32; ssn = beat $ 1 % 64

-- | Returns a 'Rhythm' consisting of @n@ consequtive 'Beat's
wns, hns, qns, ens, sns, tsns, ssns :: Int -> Rhythm Beat
wns  n = foldr (<>) (measure []) $ map (const wn ) [1..n]
hns  n = foldr (<>) (measure []) $ map (const hn ) [1..n]
qns  n = foldr (<>) (measure []) $ map (const qn ) [1..n]
ens  n = foldr (<>) (measure []) $ map (const en ) [1..n]
sns  n = foldr (<>) (measure []) $ map (const sn ) [1..n]
tsns n = foldr (<>) (measure []) $ map (const tsn) [1..n]
ssns n = foldr (<>) (measure []) $ map (const ssn) [1..n]

-- | Common "rhythmic words" in western music are patterns of 3
--   eight and sixteenth notes that last (1 % 4) to the meter.
en_sn_sn = en <> sn  <> sn -- 𝆹𝅥𝅮𝆹𝅥𝅯𝆹𝅥𝅯
sn_sn_en = sn <> sn  <> en -- 𝆹𝅥𝅯𝆹𝅥𝅯𝆹𝅥𝅮
sn_en_sn = sn <> en  <> sn -- 𝆹𝅥𝅯𝆺𝅥𝅮𝆹𝅥𝅯
den_sn   = dotted en <> sn -- 𝆺𝅥𝅮.𝆹𝅥𝅯
sn_den   = sn <> dotted en -- 𝆹𝅥𝅯𝆺𝅥𝅮.

-- | A generalized tuplet, inspired by 'An Algebra of Music' by Paul Hudak.
tuplet :: Integer -> Integer -> Rhythm Beat -> Rhythm Beat
tuplet n d = fmap $ (*)(n % d)

-- | A common usages of the generalized tuplet.
dotted, triplet :: Rhythm1 -> Rhythm1
dotted  = tuplet 3 2
triplet = tuplet 2 3
