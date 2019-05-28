
{-|
  Module      : MC.Songbook.AllTheThingsYouAre
  Description : A tribute to the greatest Hip Hop producer of all time;
                Mr. J Dillalude.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Songbook.DillaFeel101 where

import MC.Midi.Compose
import MC.Lib.Western
import Data.Ratio

-- In Dilla inspired drum grooves, we often need a more fine grained shuffle
-- variant. This n-tuplet shuffle is formally refered to as "The Dilla Feel",
-- and this function obtains the variant by trading a weigted discrete
-- distribution on beats for another distribution of the same lenght.
-- The distributions are represented as lists of integers and must be non-empty,
jDilla :: Beat -> [Integer] -> [Integer] -> Rhythm1 -> Rhythm1
jDilla b dIn dOut r = withSignature (signature r) $ fst $ jScale ratios r'
  where
    beatsIn    = sum dIn
    ratioInOut = beatsIn % sum dOut
    ratios     = map ((*ratioInOut) . fromIntegral) dOut
    m          = (duration r / b) * (beatsIn % 1)
    (n, d)     = (numerator m, denominator m)
    r'         = withSignature jSigna r
    jSigna     =
      foldr1 Shift $ take (fromIntegral (n `div` d) + 1) $
      concat $ repeat $ map (Times 1) $ map (\k -> (k % 1) * (1 % beatsIn) * b) $
      map fromIntegral dIn
    jScale (n : ns) (Measure  bs) =
      (measure $ map ((*)n) bs, ns ++ [n])
    jScale      ns  (r1  :|:  r2) =
      let (r1', ns'') = jScale ns   r1
          (r2', ns' ) = jScale ns'' r2
      in  (r1' :|: r2', ns')
    jScale      ns  (r1  :-:  r2) =
      let (r1', ns'') = jScale ns   r1
          (r2', ns' ) = jScale ns'' r2
      in  (r1' :-: r2', ns')
    jScale _        _             = (beat 0, mempty)

-- Try changing the arguments to "jDilla".
hh1, hh2, bd, sd :: AbstractPhrase
hh1 = rhythm $ qns 4
hh2 = rhythm $ jDilla (2 % 8) [1, 1] [4, 3] $ ens 8
bd  = rhythm $ measure [hn + en, hn - en]
sd  = (silence <> hit <> silence <> hit) :<: qns 4

dilla101 :: MidiComposition ()
dilla101 = do
  instrument (closedHihat   32); voice2midi $ play' (1 % 1) hh1
  instrument (closedHihat   28); voice2midi $ play' (4 % 5) hh2
  instrument (acousticSnare  9); voice2midi $ play' (3 % 4) sd
  instrument (electricSnare  8); voice2midi $ play' (1 % 1) sd
  instrument (bassDrum      44); voice2midi $ play' (1 % 1) bd
  where
    play' p = play . times 12 . velocity p

main :: IO ()
main =
  midiMC2File "./dilla.mid" 90 c_ lydian dilla101
