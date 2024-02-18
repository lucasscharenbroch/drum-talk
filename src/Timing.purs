module Timing where

import Prelude
import Words
import Data.Either

data TimedGroup = TimedGroup (Array TimedGroup) Duration
                | TimedNote Note Duration

timeify :: Array Word -> Either String (Array TimedGroup)
timeify _ = Left "" -- TODO