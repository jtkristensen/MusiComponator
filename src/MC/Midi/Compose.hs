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
                       , module MC.Lib.Rhythm.Western
                       , module MC.Lib.Harmony.Western
                       , module MC.Lib.Phrasing.Core
                       , module MC.Lib.Phrasing.Western
                       , (%)
                       , modify
                       )
       where

import MC.Core
import MC.Midi.Core
import MC.Midi.Instrument
import MC.Lib.Rhythm.Western
import MC.Lib.Harmony.Western
import MC.Lib.Phrasing.Core
import MC.Lib.Phrasing.Western
import Control.Monad.State (modify)
import Data.Ratio
