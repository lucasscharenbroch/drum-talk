module Main where

import Data.Either
import Drawable
import Note
import Parse
import Prelude
import Util
import Word

import Sticking (alternateSticking)
import Timing (dissolvePow2Tuplets, splitEvenTuplets, timeify)

compile :: Settings -> String -> Either String (Array DrawableMeasure)
compile settings = parse settings
               >=> timeify settings
               >=> (pure <<< alternateSticking)
               >=> (pure <<< dissolvePow2Tuplets)
               >=> (pure <<< splitEvenTuplets)
               >=> toDrawable settings

defaultSettings :: Settings
defaultSettings =
    { timeSig: TimeSig n4 n4
    , minDuration: d32
    , defShort: d16
    , defLong: d8
    , defGroupDuration: d4
    , defNote: { numGraceNotes: n0
               , stroke: Tap
               , articulation: Normal
               , stick: NeutralStick
               }
    }
