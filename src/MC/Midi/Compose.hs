{-|
  Module      : MC.Midi.Compose
  Description : Reexports for midi-composition.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Midi.Compose ( module MC.Core
                       , module MC.Midi.Core
                       , module MC.Midi.Instrument
                       , (%)
                       )
       where

import MC.Core
import MC.Midi.Core
import MC.Midi.Instrument
import Data.Ratio
