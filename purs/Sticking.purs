module Sticking where

import Prelude
import Timing
import Util

import Debug (spy)

alternateSticking :: Array TimedGroup -> Array TimedGroup
alternateSticking = id -- TODO
    where _ = spy "" "alt"
