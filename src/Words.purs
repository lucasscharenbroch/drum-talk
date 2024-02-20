module Words where

import Prelude

import Data.Eq
import Data.EuclideanRing
import Data.HeytingAlgebra
import Data.Natural
import Data.Rational
import Data.Tuple
import Data.Ord
import Data.Maybe
import Tree

data Stroke = Tap
            | Double
            | Gock
            | Buzz
            | LongRoll

data Articulation = Normal
                  | Accent
                  | Marcato

-- The length of a note, relative to the beat, or
-- to the length of surrounding notes;
-- the denominator should be a power of 2
newtype Duration = Duration Rational

derive newtype instance Ord Duration
derive newtype instance Ring Duration

d4 = Duration (1 % 4) :: Duration
d8 = Duration (1 % 8) :: Duration
d16 = Duration (1 % 16) :: Duration

data Stick = SLeft | SRight

-- A description for how to play a note (no time information involved)
type Note =
    { numGraceNotes :: Natural
    , stroke :: Stroke
    , articulation :: Articulation
    , stick :: Stick
    }

-- A note with duration relative to its neighbors
data WeightedNote = WeightedNote Note Natural
                  | WeightedRest Natural

-- A point in time, relative to a measure
newtype Time = Time Rational

-- Corresponds to a "word" in Drum-Talk
data Word = AbsoluteWord Time -- implicit duration
          | RelativeWord (Tree WeightedNote) Duration -- implicit time
          | CompleteWord Time (Tree WeightedNote) Duration

mkNote :: Natural -> Stroke -> Articulation -> Stick -> Note
mkNote numGraceNotes stroke articulation stick =
    { numGraceNotes
    , stroke
    , articulation
    , stick
    }
