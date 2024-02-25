module Word where

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
import Note

import Data.Generic.Rep
import Data.Show.Generic
import Data.Show


-- The length of a note, relative to the beat, or
-- to the length of surrounding notes;
-- the denominator should be a power of 2
newtype Duration = Duration Rational

derive instance genericDuration :: Generic Duration _
instance Show Duration where
    show = genericShow

derive newtype instance Ord Duration
derive newtype instance Ring Duration
derive newtype instance Semiring Duration
derive newtype instance EuclideanRing Duration

d0 = Duration (0 % 4) :: Duration
d4 = Duration (1 % 4) :: Duration
d8 = Duration (1 % 8) :: Duration
d16 = Duration (1 % 16) :: Duration
d32 = Duration (1 % 32) :: Duration

-- A time-offset, relative to the start of a measure or beat
data Time = MeasureOffset Rational
          | BeatOffset Rational

derive instance genericTime :: Generic Time _
instance Show Time where
    show = genericShow

-- A a time-offset, relative to the start of a measure
newtype MeasureTime = MeasureTime Rational

derive instance genericMeasureTime :: Generic MeasureTime _
instance Show MeasureTime where
    show = genericShow

-- Corresponds to a "word" in Drum-Talk
data Word = AbsoluteWord Time Note -- implicit duration
          | RelativeWord (Tree WeightedNote) Duration -- implicit time
          | CompleteWord Time (Tree WeightedNote) Duration

derive instance genericWord :: Generic Word _
instance Show Word where
    show = genericShow

-- More-narrow word wrapper-types

data AWord = AWord Time Note -- <=>. AbsoluteWord
data RWord = RWord (Tree WeightedNote) Duration -- <=> RelativeWord
data CWord = CWord Time (Tree WeightedNote) Duration -- <=> CompleteWord

derive instance genericAword :: Generic AWord _
instance Show AWord where
    show = genericShow

derive instance genericRword :: Generic RWord _
instance Show RWord where
    show = genericShow

derive instance genericCword :: Generic CWord _
instance Show CWord where
    show = genericShow

class ToWord a where
    toWord :: a -> Word

instance ToWord AWord where
    toWord (AWord time note) = AbsoluteWord time note

instance ToWord RWord where
    toWord (RWord tree duration) = RelativeWord tree duration

instance ToWord CWord where
    toWord (CWord time tree duration) = CompleteWord time tree duration
