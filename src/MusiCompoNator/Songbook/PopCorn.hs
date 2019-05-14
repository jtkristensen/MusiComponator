
module MusiCompoNator.Songbook.PopCorn where

import MusiCompoNator.Composition
import MusiCompoNator.Core
import MusiCompoNator.GeneralMidi
import ZMidi.Core
import Data.Ratio

bassIntro :: Voice ()
bassIntro = singleV $
  (foldl (<>) (rest 0) $ take 4 $ repeat $ bassOstinato 1 4)

bassRiff :: (Scale -> Scale) -> [Int] -> Voice ()
bassRiff f xs =
  singleV $
  liftH (map $ Mode f) $
  foldr1 (<>) $ zipWith bassOstinato xs [4, 4, 2, 2, 2, 2]

-- chord step -> number of beats -> ostinato
bassOstinato :: Int -> Int -> Phrase1
bassOstinato c' n' = liftH (map (Mode $ map (down . down))) $ f c' n'
  where f c 1 = (arpeggio  c [i_, iii_, v_, i]) :<: sns 4
        f c n = ((arpeggio c [i_, i, v_]) :<: (ens 1 <> sns 2)) <> f c (n - 1)

composition = closeMC

-- Served as a file.
main :: IO ()
main = do
  (f, ()) <- return $ runMidiComposition (aeolian (flat b)) composition
  writeMidi "./popcorn.mid" f
