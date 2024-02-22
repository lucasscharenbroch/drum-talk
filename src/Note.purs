module Note where

import Data.Natural (Natural)

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

mkNote :: Natural -> Stroke -> Articulation -> Stick -> Note
mkNote numGraceNotes stroke articulation stick =
    { numGraceNotes
    , stroke
    , articulation
    , stick
    }
