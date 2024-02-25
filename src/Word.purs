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

-- The length of a note, relative to the beat, or
-- to the length of surrounding notes;
-- the denominator should be a power of 2
newtype Duration = Duration Rational

derive newtype instance Ord Duration
derive newtype instance Ring Duration
derive newtype instance Semiring Duration
derive newtype instance EuclideanRing Duration

d0 = Duration (0 % 4) :: Duration
d4 = Duration (1 % 4) :: Duration
d8 = Duration (1 % 8) :: Duration
d16 = Duration (1 % 16) :: Duration

-- A time-offset, relative to the start of a measure or beat
data Time = MeasureOffset Rational
          | BeatOffset Rational

-- A a time-offset, relative to the start of a measure
newtype MeasureTime = MeasureTime Rational

-- Corresponds to a "word" in Drum-Talk
data Word = AbsoluteWord Time Note -- implicit duration
          | RelativeWord (Tree WeightedNote) Duration -- implicit time
          | CompleteWord Time (Tree WeightedNote) Duration

-- More-narrow word wrapper-types

data AWord = AWord Time Note -- <=>. AbsoluteWord
data RWord = RWord (Tree WeightedNote) Duration -- <=> RelativeWord
data CWord = CWord Time (Tree WeightedNote) Duration -- <=> CompleteWord

class ToWord a where
    toWord :: a -> Word

instance ToWord AWord where
    toWord (AWord time note) = AbsoluteWord time note

instance ToWord RWord where
    toWord (RWord tree duration) = RelativeWord tree duration

instance ToWord CWord where
    toWord (CWord time tree duration) = CompleteWord time tree duration
