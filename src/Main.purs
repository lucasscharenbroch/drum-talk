module Main where

import Data.Either
import Parse
import Prelude
import Timing

import Effect (Effect)
import Effect.Console (log)
import Sticking (alternateSticking)
import Timing (TimedGroup(..))

compile :: Settings -> String -> Either String (Array TimedGroup)
compile settings = (pure <<< alternateSticking) <=< timeify settings <=< parse settings

main :: Effect Unit
main = do
  log "Hello, world!"
