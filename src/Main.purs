module Main where

import Data.Either
import Parse
import Prelude
import Timing

import Effect (Effect)
import Effect.Console (log)
import Sticking (alternateSticking)
import Timing (TimedNote(..))

compile :: String -> Either String (Array TimedNote)
compile = (pure <<< alternateSticking) <=< timeify <=< parse

main :: Effect Unit
main = do
  log "Hello, world!"
