module Note where

import Data.Natural (Natural)

import Data.Generic.Rep
import Data.Show.Generic
import Data.Show
import Data.Eq

data Stroke = Tap
            | Double
            | Gock
            | Buzz
            | LongRoll

derive instance genericStroke :: Generic Stroke _
instance Show Stroke where
    show = genericShow

data Articulation = Normal
                  | Accent
                  | Marcato

derive instance genericArtic :: Generic Articulation _
instance Show Articulation where
    show = genericShow

data Stick = StrongLeft
           | StrongRight
           | WeakLeft
           | WeakRight

derive instance Eq Stick

derive instance genericStick :: Generic Stick _
instance Show Stick where
    show = genericShow

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

derive instance genericWeighted :: Generic WeightedNote _
instance Show WeightedNote where
    show = genericShow

mkNote :: Natural -> Stroke -> Articulation -> Stick -> Note
mkNote numGraceNotes stroke articulation stick =
    { numGraceNotes
    , stroke
    , articulation
    , stick
    }
