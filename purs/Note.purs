module Note where

import Data.Natural (Natural)

import Data.Eq

data Stroke = Tap
            | Double
            | Gock
            | Buzz
            | LongRoll

data Articulation = Normal
                  | Accent
                  | Marcato

data Stick = StrongLeft
           | StrongRight
           | WeakLeft
           | WeakRight
           | NeutralStick

derive instance Eq Stick

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

getWeight :: WeightedNote -> Natural
getWeight (WeightedNote _ w) = w
getWeight (WeightedRest w) = w

mkNote :: Natural -> Stroke -> Articulation -> Stick -> Note
mkNote numGraceNotes stroke articulation stick =
    { numGraceNotes
    , stroke
    , articulation
    , stick
    }
