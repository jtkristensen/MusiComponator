module MusiCompoNator.Lib.Rhythm where

import MusiCompoNator.Core

-- TODO : updata sig.



-- TODO : Better version.
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
-- jDilla :: Meter -> [Integer] -> (Rhythm Beat -> Rhythm Beat)
-- jDilla m ks r =
--   fromTime s $ fst $ jScale (concat (repeat ks)) $ jTuplet $ fromTime s' r
--   where
--     (n, d) = m
--     k      = numerator $ (foldl lcm d $ map denominator jSigna) % d
--     l   = (fromIntegral $ length ks)
--     s   = signature r
--     s'  = Shift (signature $ beat $ ((meter2beat m) / l)) s'
--     jSigna  = map (\k -> k % (sum ks) * (meter2beat m)) ks
--     jTuplet = updateSig . (fmap $ tuplet n k)
--     jScale (n : ns) (Measure _ bs) = (measure $ map ((*) $ fromIntegral n) bs, ns)
--     jScale ns (Repeat 0 _)   = (measure [], ns)
--     jScale ns (Repeat l r)   =
--       let (rb, ns') = jScale ns r in  first (rb:|:) $ jScale ns' (Repeat (l - 1) r)
--     jScale ns (r1 :|: r2)    =
--       let (rb, ns') = jScale ns r1 in  first (rb:|:) $ jScale ns' r2
--     jScale ns (r1 :-: r2)    =
--       let (rb, ns') = jScale ns r1 in  first (rb:-:) $ jScale ns' r2
--     jScale _ _ = error "does not happen by construction."

-- Common shuffles
-- shuffle8, shuffle16 :: Rhythm Beat -> Rhythm Beat
-- shuffle8  = jDilla (2,  8) [2, 1]
-- shuffle16 = jDilla (2, 16) [2, 1]
