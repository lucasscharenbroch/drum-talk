module Main where

import Data.Either
import Note
import Parse
import Prelude
import Timing
import Util
import Word

import Control.Comonad.Env (local)
import Data.Rational ((%))
import Effect (Effect)
import Effect.Console (log)
import Sticking (alternateSticking)
import Timing (TimedGroup(..))
import Debug (spy)
import Data.Array (intercalate)

compile :: Settings -> String -> Either String (Array TimedGroup)
compile settings = (pure <<< alternateSticking) <=< timeify settings <=< parse settings

cases :: Array String
cases = [
  "1 2 3 4",
  "1 e &",
  "a a a",
  "ah ah a",
  "and ah one e and a two e and",
  "1 [2] &",
  "flamtap",
  "ptff ft ft f",
  "Pataflafla",
  "_",
  "da",
  "xyz",
  "{z} tap",
  "{z} one",
  "twentyfive"
]

defaultSettings :: Settings
defaultSettings =
    { timeSig: TimeSig (4 % 1)
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
  log <<< intercalate "\n" <<< map show $ map (parse defaultSettings) cases
