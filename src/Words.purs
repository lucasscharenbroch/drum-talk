module Words where

import Data.Natural
import Data.Tuple
import Data.Rational

-- The lowest building-block of a drum note;
-- something that can be done with one stick
-- that is a valid grace note
data Atom = Tap
          | Buzz
          | Gock

-- A valid non-grace note (which itself has no grace notes)
data Molecule = AtomicMolecule Atom
              | DoubleStroke
              | LongRoll

data Articulation = NormalHit
                  | Accent
                  | Marcato

newtype Duration = Duration Rational

data Stick = Left | Right

data GraceNote = GraceNote Atom Stick

-- A main-note, optional grace notes, and articulation
type Note =
    { graceNotes :: Array GraceNote
    , mainNote :: Molecule
    , articulation :: Articulation
    , duration :: Duration
    , stick :: Stick
    }

-- A point in time, relative to a measure
newtype Time = Time Rational

newtype AbsoluteWord = AbsoluteWord Time

data RelativeWord = RelativeWord (Array Note) Duration
