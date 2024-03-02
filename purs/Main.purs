module Main where

import Data.Either
import Note
import Parse
import Prelude
import Timing
import Util
import Word
import Drawable

import Control.Comonad.Env (local)
import Data.Array (intercalate)
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Sticking (alternateSticking)
import Timing (TimedGroup(..))

compile :: Settings -> String -> Either String (Array DrawableMeasure)
compile settings = parse settings
               >=> timeify settings
               >=> (pure <<< alternateSticking)
               >=> toDrawable settings

cases :: Array String
cases = [
  -- "2 3 2 2"
  "paradiddle"
  -- "<2>(tap tap (tap tap))"
  -- "e"
  -- "1 e &"
  -- "a a a a a a a a a a"
  -- "ah ah a",
  -- "and ah one e and a two e and",
  -- "1 [2] &"
  -- "PtfF ft ft f"
  -- "da"
  -- "duh",
  -- "duh da duh da duh da duh da da duh da duh",
  -- "flamtap",
  -- "FlamTap",
  -- "Flam---Tap",
  -- "f-t",
  -- "PataflaflA",
  -- "{z}tap",
  -- "{z}one",
  -- "twentyfive"
  -- "twen-{z}ty-five",
  -- "{zq}"
  -- "one two three 1e&a"
  -- "tap tap flamtap flam flam tap flam"
  -- "[1] & 2e&"
  -- "(tap tap tap)"
  -- "(da duh duh da)"
  -- "<4> ta"
  -- "<16> (<8> tuh ta ta)"
  -- "(({\"}tuh tuh tuh) ta)"
  -- "(one two)"
  -- "papapadd"
  -- "padddd"
  -- "paradiddlediddle"
  -- "<32> paraParaParadiddle"
  -- (bad)
  -- "paddd"
  -- "_",
  -- "xyz",
]

test :: Either String Int -> Int
test esa = case esa of
    (Left s) -> 3
    (Right i) -> i

defaultSettings :: Settings
defaultSettings =
    { timeSig: TimeSig n4 n4
    , minDuration: d32
    , defDuration: d8
    , defNote: { numGraceNotes: n0
               , stroke: Tap
               , articulation: Normal
               , stick: WeakRight
               }
    }

main :: Effect Unit
main = do
  -- log <<< intercalate "\n" <<< map show $ map (parse defaultSettings) cases
  log <<< intercalate "\n" $ map (show <<< compile defaultSettings) cases
