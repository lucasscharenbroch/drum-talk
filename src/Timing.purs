module Timing where

import Prelude
import Words
import Data.Either

data TimedNote = TimedNote Note Duration

timeify :: Array Word -> Either String (Array TimedNote)
timeify _ = Left "" -- TODO