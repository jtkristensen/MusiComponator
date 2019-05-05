
module MusiCompoNator.Examples.Twinkle where

import MusiCompoNator.Composition
import MusiCompoNator.Core

melody :: Phrase a Prim Beat
melody = withSignature (Times 12 1) $ a <> b <> c <> c <> a <> b
  where a = line [i,   i,    v,   v,  vi,  vi,  v] :<: r -- Twinkle twinkle ..
        b = line [iv, iv,  iii, iii,  ii,  ii,  i] :<: r -- How I wonder ..
        c = line [ v,  v,   iv,  iv, iii, iii, ii] :<: r -- Up above the ..
        r = qns 6 <> hns 1
