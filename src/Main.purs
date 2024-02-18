module Main where

import Data.Either
import Parse
import Prelude
import Timing

import Effect (Effect)
import Effect.Console (log)
import Sticking (alternateSticking)
import Timing (TimedGroup(..))

compile :: String -> Either String (Array TimedGroup)
compile = (pure <<< alternateSticking) <=< timeify <=< parse

main :: Effect Unit
main = do
  log "Hello, world!"
