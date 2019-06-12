{-|
  Module      : MC.Lib.Phrasing.Western
  Description : Core library functions concerned with phrasing.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Lib.Phrasing.Western where

import MC.Core
import MC.Lib.Phrasing.Core
import Data.Ratio ((%))

-- | Italian for "as loud as possible".
fff :: Phrase p b -> Phrase p b
fff = velocity 1

-- | Itallian for "loud"
fortissimo :: Phrase p b -> Phrase p b
fortissimo = velocity (7 % 8)

-- | Itallian for "somewhat loud".
forte :: Phrase p b -> Phrase p b
forte = velocity (6 % 8)

-- | Not too loud.
mezzoForte :: Phrase p b -> Phrase p b
mezzoForte = velocity (5 % 8)

-- | Not too quiet.
mezzoPiano :: Phrase p b -> Phrase p b
mezzoPiano = velocity (4 % 8)

-- | Quietly.
piano :: Phrase p b -> Phrase p b
piano = velocity (3 % 8)

-- | Very quiet.
pianissimo :: Phrase p b -> Phrase p b
pianissimo = velocity (2 % 8)

-- | As quiet as possible.
ppp :: Phrase p b -> Phrase p b
ppp = velocity (1 % 8)

-- | A very short staccato.
dash :: Phrase p b -> Phrase p b
dash = staccato (1 % 128)

-- | A somewhat short staccato.
dot :: Phrase p b -> Phrase p b
dot = staccato (1 % 32)

-- More to come
