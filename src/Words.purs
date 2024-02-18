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

import Data.Int.Bits ((.&.))

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

d4 = Duration (1 % 4) :: Duration
d8 = Duration (1 % 8) :: Duration
d16 = Duration (1 % 16) :: Duration

data Stick = SLeft | SRight

-- A main-note, optional grace notes, and articulation
type Note =
    { numGraceNotes :: Natural
    , stroke :: Stroke
    , articulation :: Articulation
    , duration :: Duration
    , stick :: Stick
    }

-- A point in time, relative to a measure
newtype Time = Time Rational

data Word = AbsoluteWord Time -- implicit duration
          | RelativeWord (Array Note) Duration -- implicit time
          | CompleteWord Time (Array Note) Duration

mkNote :: Natural -> Stroke -> Articulation -> Duration -> Stick -> Note
mkNote nGraceNotes stroke artic duration stick =
    { numGraceNotes: nGraceNotes
    , stroke: stroke
    , articulation: artic
    , duration: duration
    , stick: stick
    }
