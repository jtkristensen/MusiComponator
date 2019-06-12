{-|
  Module      : MC.Lib.Phrasing.Core
  Description : Core library functions concerned with phrasing.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Lib.Phrasing.Core where

import MC.Core

-- | Change the 'Velocity' at which a phrase should be played.
velocity :: Percentage -> Phrase p b -> Phrase p b
velocity q = modifyCtrl $ Velocity q

-- | Sets on a bound on the sounding 'Duration' of the notes in a 'Phrase'.
staccato :: Duration -> Phrase p b -> Phrase p b
staccato b = modifyCtrl $ Staccato b

-- | Play the notes more connected, and at a more gentle velocity.
legato :: Phrase p b -> Phrase p b
legato = modifyCtrl $ Legato

-- | Change the ring modulator.
modulate :: Percentage -> Phrase p b -> Phrase p b
modulate = modifyCtrl . Modulator

-- Should this go to a track instead ?
reverb :: Percentage -> Phrase p b -> Phrase p b
reverb = modifyCtrl . Reverb
