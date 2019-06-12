{-|
  Module      : MC.Lib.Harmony.Western
  Description : A library that describes common consructions in western music.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Lib.Harmony.Western where

import MC.Core
import Control.Monad.State

-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
c__, c_, c, c', c'', d__, d_, d, d', d''           :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
cb__, cb_, cb, cb', cb'', db__, db_, db, db', db'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
cs__, cs_, cs, cs', cs'', ds__, ds_, ds, ds', ds'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
e__, e_, e, e', e'', f__, f_, f, f', f''          :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
eb__, eb_, eb, eb', eb'', fb__, fb_, fb, fb', fb'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
es__, es_, es, es', es'', fs__, fs_, fs, fs', fs'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
g__, g_, g, g', g'', a__, a_, a, a', a''            :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
gb__, gb_, gb, gb', gb'', ab__, ab_, ab, ab', ab'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
gs__, gs_, gs, gs', gs'', as__, as_, as, as', as'' :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
b__, b_, b, b', b''                               :: Relative Pitch
-- | A name for a particular 'Pitch' in western music.
--   This particular pitch is 'Relative' to the 'chromatic' 'Scale',
--   and as such, it may be seen as a 'Relative Pitch'
--   Note also, that the symbol "@,@" in Haskell has a special meaning
--   So, we use "@_@" instead.
bb__, bb_, bb, bb', bb'',bs__, bs_, bs, bs', bs'' :: Relative Pitch

c''  = up   <$> c' ; d''  = up   <$> d' ; e''  = up   <$> e'; f'' = up   <$> f'
g''  = up   <$> g' ; a''  = up   <$> a' ; b''  = up   <$> b'
c'   = up   <$> c  ; d'   = up   <$> d  ; e'   = up   <$> e ; f'  = up   <$> f
g'   = up   <$> g  ; a'   = up   <$> a  ; b'   = up   <$> b
c    = absPitch 0  ; d    = absPitch 2  ; e    = absPitch  4; f   = absPitch 5
g    = absPitch 7  ; a    = absPitch 9  ; b    = absPitch 11
c_   = down <$> c  ; d_   = down <$> d  ; e_   = down <$> e ; f_  = down <$> f
g_   = down <$> g  ; a_   = down <$> a  ; b_   = down <$> b
c__  = down <$> c_ ; d__  = down <$> d_ ; e__  = down <$> e_; f__ = down <$> f_
g__  = down <$> g_ ; a__  = down <$> a_ ; b__  = down <$> b_
cb'' = c'' <> flat ; db'' = d'' <> flat ; eb'' = e'' <> flat; fb'' = f'' <> flat
gb'' = g'' <> flat ; ab'' = a'' <> flat ; bb'' = b'' <> flat
cb'  = c'  <> flat ; db'  = d'  <> flat ; eb'  = e'  <> flat; fb'  = f'  <> flat
gb'  = g'  <> flat ; ab'  = a'  <> flat ; bb'  = b'  <> flat
cb   = c   <> flat ; db   = d   <> flat ; eb   = e   <> flat; fb   = f <> flat
gb   = g   <> flat ; ab   = a   <> flat ; bb   = b   <> flat
cb_  = c_  <> flat ; db_  = d_  <> flat ; eb_  = e_  <> flat; fb_  = f_  <> flat
gb_  = g_  <> flat ; ab_  = a_  <> flat ; bb_  = b_  <> flat
cb__ = c__ <> flat ; db__ = d__ <> flat ; eb__ = e__ <> flat; fb__ = f__ <> flat
gb__ = g__ <> flat ; ab__ = a__ <> flat ; bb__ = b__ <> flat
cs'' = c'' <> sharp; ds'' = d'' <> sharp; es'' = e'' <> sharp; fs'' = f'' <> sharp
gs'' = g'' <> sharp; as'' = a'' <> sharp; bs'' = b'' <> sharp
cs'  = c'  <> sharp; ds'  = d'  <> sharp; es'  = e'  <> sharp; fs'  = f'  <> sharp
gs'  = g'  <> sharp; as'  = a'  <> sharp; bs'  = b'  <> sharp
cs   = c   <> sharp; ds   = d   <> sharp; es   = e   <> sharp; fs   = f <> sharp
gs   = g   <> sharp; as   = a   <> sharp; bs   = b   <> sharp
cs_  = c_  <> sharp; ds_  = d_  <> sharp; es_  = e_  <> sharp; fs_  = f_  <> sharp
gs_  = g_  <> sharp; as_  = a_  <> sharp; bs_  = b_  <> sharp
cs__ = c__ <> sharp; ds__ = d__ <> sharp; es__ = e__ <> sharp; fs__ = f__ <> sharp
gs__ = g__ <> sharp; as__ = a__ <> sharp; bs__ = b__ <> sharp

-- | A common operaions on 'Pitch' in westen music.
sharp, flat :: Relative Pitch
sharp = do
  get >>= (\(o, s') -> absPitch $ root s' + o) . (\s -> locate (root s + 1) s)
flat  = do
  get >>= (\(o, s') -> absPitch $ root s' + o) . (\s -> locate (root s - 1) s)

-- | Common diatonic 'mode's.
ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Pitch -> Scale
ionian     p = Scale p [2, 2, 1, 2, 2, 2, 1]
dorian     p = Scale p [2, 1, 2, 2, 2, 1, 2]
phrygian   p = Scale p [1, 2, 2, 2, 1, 2, 2]
lydian     p = Scale p [2, 2, 2, 1, 2, 2, 1]
mixolydian p = Scale p [2, 2, 1, 2, 2, 1, 2]
aeolian    p = Scale p [2, 1, 2, 2, 1, 2, 2]
locrian    p = Scale p [1, 2, 2, 1, 2, 2, 2]

-- | Left and right motions for the inner and outer circles of fifths.
c5outerr, c5outerl, c5innerr, c5innerl :: Voice ()
c5outerr = modify $ \s -> s { vsKey = (relMode (major4 <> v))     (vsKey s)}
c5outerl = modify $ \s -> s { vsKey = (relMode (iv_ <> perfect4)) (vsKey s)}
c5innerr = relativeMajor >> c5outerr >> relativeMinor
c5innerl = relativeMajor >> c5outerl >> relativeMinor

-- | Relative major and minor 'mode's
relativeMajor, relativeMinor           :: Voice ()
relativeMajor = modify $ \s -> s {vsKey = relMode iii (vsKey s)}
relativeMinor = modify $ \s -> s {vsKey = relMode vi_ (vsKey s)}

-- | Alters a single interval on diatonic 'mode's
minor2, major2, minor3, major3, perfect4, major4 :: Chord
minor5, perfect5, minor6, major6, minor7, major7 :: Chord
perfect8, minor9, major9, minor10, major10       :: Chord
perfect11, major11, minor12, perfect12           :: Chord
minor13, major13                                 :: Chord
minor2    = modifyDiatonic 2  1
major2    = modifyDiatonic 2  2
minor3    = modifyDiatonic 3  3
major3    = modifyDiatonic 3  4
perfect4  = modifyDiatonic 4  5
major4    = modifyDiatonic 4  6
minor5    = modifyDiatonic 5  6
perfect5  = modifyDiatonic 5  7
minor6    = modifyDiatonic 6  8
major6    = modifyDiatonic 6  9
minor7    = modifyDiatonic 7 10
major7    = modifyDiatonic 7 11
perfect8  = modifyDiatonic 8 12
minor9    = minor2
major9    = major2
minor10   = minor3
major10   = major3
perfect11 = perfect4
major11   = major4
minor12   = minor5
perfect12 = perfect5
minor13   = minor6
major13   = major6

-- | A common 'Chord' annotation.
b2, b3, b5, s5, b9, s9, s11, b13 :: Chord
b2  = minor2
b3  = minor3
b5  = minor5
s5  = modifyDiatonic 5 8
b9  = minor9
s9  = major9
s11 = major11
b13 = minor13

-- | A Simple 'Chord' transformations.
major, minor :: Chord -> Chord
major = mode $ ionian  . root
minor = mode $ aeolian . root

-- | A common 'Chord' type.
maj7, maj9, maj11, dom7, min7, min9, min11 :: Chord
-- | A common 'Chord' type.
min7b5, min7b9, b9b13 :: Chord
maj7   = major3 <> perfect5 <> major7
maj9   = maj7   <> major9
maj11  = maj9   <> major11
dom7   = major3 <> perfect5 <> minor7
min7   = minor3 <> perfect5 <> minor7
min9   = min7   <> major9
min11  = min9   <> perfect11
min7b5 = min7   <> b5
min7b9 = min7   <> b9
b9b13  = dom7   <> b9 <> b13

-- | Modifies the m'th step of a scale s', to be at exactly s semitones
--   distance from the root of s'.
modifyDiatonic :: Int -> Semitone -> Chord
modifyDiatonic 2 s = do
  (Scale r t) <- get
  let (x, xs) = (head t , tail  t)
      (y, ys) = (head xs, tail xs)
  put $ Scale r $ s : (y + (x - s)) : ys
  return r
modifyDiatonic m s = do
  (Scale _ t) <- get
  if     m < 2
    then modify invertl >> modifyDiatonic (m + 1) (s + head t) >> modify invertr
    else modify invertr >> modifyDiatonic (m - 1) (s - head t) >> modify invertl
  relRoot

-- | Short hand for the 'Relative' 'Pitch' from step @n@ of some underlying 'Scale'.
rp :: Int -> Relative Pitch
rp n = (modify $ step n) >> relRoot

-- | A common 'Scale' abstrations (Nasville style).
i, ii, iii, iiv, iv, v, vi, vii, viii, iix, ix, x              :: Relative Pitch
xi, xii, xiii                                                  :: Relative Pitch

i  = rp  1; ii  = rp  2; iii  = rp  3; iiv = rp 3; iv = rp 4; v = rp 5
vi = rp  6; vii = rp  7; viii = rp  8; iix = rp 8; ix = rp 9; x = rp 10
xi = rp 11; xii = rp 12; xiii = rp 13

-- | A common scale abstrations (Nasville style, one octave above the root).
i', ii', iii', iiv', iv', v', vi', vii', viii', iix', ix', x'  :: Relative Pitch
xi', xii', xiii'                                               :: Relative Pitch

i'     = fmap up    i;  ii'  = fmap up   ii;  iii' = fmap up iii
iv'    = fmap up   iv;   v'  = fmap up    v;   vi' = fmap up  vi
vii'   = fmap up  vii; viii' = fmap up  iix;   ix' = fmap up  ix
x'     = fmap up    x;  xi'  = fmap up   xi;  xii' = fmap up xii
xiii'  = fmap up xiii; iix'  = fmap up viii;  iiv' = fmap up iii

-- | A common scale abstrations (Nasville style, two octaves above the root).
i'', ii'', iii'', iiv'', iv'', v'', vi'', vii'', viii'', iix'' :: Relative Pitch
ix'', x'', xi'', xii'', xiii''                                 :: Relative Pitch

i''     = fmap up    i';  ii''  = fmap up   ii';  iii'' = fmap up iii'
iv''    = fmap up   iv';   v''  = fmap up    v';   vi'' = fmap up  vi'
vii''   = fmap up  vii'; viii'' = fmap up  iix';   ix'' = fmap up  ix'
x''     = fmap up    x';  xi''  = fmap up   xi';  xii'' = fmap up xii'
xiii''  = fmap up xiii'; iix''  = fmap up viii';  iiv'' = fmap up iii'

-- | A common scale abstrations (Nasville style, one octave below the root).
i_, ii_, iii_, iiv_, iv_, v_, vi_, vii_, viii_, iix_, ix_, x_  :: Relative Pitch
xi_, xii_, xiii_                                               :: Relative Pitch

i_     = fmap down    i;  ii_  = fmap down   ii;  iii_ = fmap down iii
iv_    = fmap down   iv;   v_  = fmap down    v;   vi_ = fmap down  vi
vii_   = fmap down  vii; viii_ = fmap down  iix;   ix_ = fmap down  ix
x_     = fmap down    x;  xi_  = fmap down   xi;  xii_ = fmap down xii
xiii_  = fmap down xiii; iix_  = fmap down viii;  iiv_ = fmap down iii

-- | A common scale abstrations (Nasville style, two octaves below the root).
i__, ii__, iii__, iiv__, iv__, v__, vi__, vii__, viii__, iix__ :: Relative Pitch
ix__, x__, xi__, xii__, xiii__                                 :: Relative Pitch

i__     = fmap down    i_;  ii__  = fmap down   ii_;  iii__ = fmap down iii_
iv__    = fmap down   iv_;   v__  = fmap down    v_;   vi__ = fmap down  vi_
vii__   = fmap down  vii_; viii__ = fmap down  iix_;   ix__ = fmap down  ix_
x__     = fmap down    x_;  xi__  = fmap down   xi_;  xii__ = fmap down xii_
xiii__  = fmap down xiii_; iix__  = fmap down viii_;  iiv__ = fmap down iii_

-- | An inversion of the triad in root position.
triad1, triad2, triad3 :: Chord -> Sequence Chord
triad1 = voicing [i, iii, v]
triad2 = voicing [iii, v, i']
triad3 = voicing [v, i', iii']

-- | An inversion of the seventh-chord in root position.
chord1, chord2, chord3, chord4 :: Chord -> Sequence Chord
chord1 = voicing [i, iii, v, vii]
chord2 = voicing [iii, v, vii, i']
chord3 = voicing [v, vii, i', iii']
chord4 = voicing [vii, i', iii', v']
