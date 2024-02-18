module Timing where

import Control.Monad.State.Trans
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Rational
import Data.Traversable
import Data.Tuple
import Data.Array
import Prelude
import Tree
import Words

import Data.Int.Bits ((.&.))
import JS.BigInt (toInt)
import Parse (Settings, TimeSig(..))

-- lower limit, upper limit, default of where a note in a mesure ends
data TimeRange = TimeRange Time Time Time

mkTimeRange1 :: Time -> TimeRange
mkTimeRange1 t = TimeRange t t t

data TimedGroup = TimedGroup (Tree WeightedNote) Duration
                | TimedNote Note Duration
                | TimedRest Duration

timeify :: Tuple Settings (Array Word) -> Either String (Array TimedGroup)
timeify (Tuple settings words) = validateSettings settings *> res
    where zero = TimeRange (Time (0 % 1)) (Time (0 % 1))
          res = evalStateT (concat <$> traverse (wordToGroup settings) words) $ zero

validateSettings :: Settings -> Either String Unit
validateSettings {timeSig: TimeSig sig} = res
    where
        sigNum = fromMaybe (-1) <<< toInt <<< numerator $ sig
        sigDenom = fromMaybe (-1) <<< toInt <<< denominator $ sig
        isPow2 x = x .&. (x - 1) == 0
        pow2Err = "Time signature's denominator should be a power of 2"
        positiveErr = "Time signature should be positive"
        res
            | sigNum <= 0 || sigDenom <= 0 = Left positiveErr
            | isPow2 sigDenom = Left pow2Err
            | otherwise = Right unit

validateStartTime :: Settings -> Time -> Either String Unit
validateStartTime {timeSig: TimeSig sig} (Time t)
    | t >= sig = Left $ "Note start-time exceeds size of measure: " <> show t
    | otherwise = Right unit

-- Calculates a new time given a start-time, duration, and signature
addDurationMod :: TimeSig -> Time -> Duration -> Time
addDurationMod (TimeSig sig) (Time t) (Duration d)
    | t + d < sig = Time $ t + d
    | otherwise = Time $ t + d - sig

subTimeMod :: TimeSig -> Time -> Time -> Duration
subTimeMod (TimeSig sig) (Time t1) (Time t2)
    | t1 - t2 < (0 % 1) = Duration $ t1 - t2 + sig
    | otherwise = Duration $ t1 - t2

noteTreeToTimedGroup :: Tree WeightedNote -> Duration -> TimedGroup
noteTreeToTimedGroup (Leaf (WeightedNote note _)) duration = TimedNote note duration
noteTreeToTimedGroup (Leaf (WeightedRest _)) duration = TimedRest duration
noteTreeToTimedGroup tree@(Internal _) duration = TimedGroup tree duration

mkDefTimeRange :: Settings -> Time -> TimeRange
mkDefTimeRange {minDuration, maxDuration, defDuration, timeSig: sig} time = TimeRange early late default
    where early = addDurationMod sig time minDuration
          late = addDurationMod sig time maxDuration
          default = addDurationMod sig time defDuration

wordToGroup :: Settings -> Word -> StateT TimeRange (Either String) (Array TimedGroup)
wordToGroup settings (AbsoluteWord time) = asserts *> res
    where
        asserts = lift $ validateStartTime settings time
        {timeSig: sig, minDuration, defDuration, maxDuration, defNote} = settings
        res = do
            (TimeRange early late _) <- get
            let space = subTimeMod sig time early -- between early end & start
            let range = subTimeMod sig late early -- net size of time range
            put $ mkDefTimeRange settings time
            let timedNote = TimedNote defNote ?duration -- can't tell the duration here
            if space > range
            then pure [TimedRest $ space - range, timedNote]
            else pure [timedNote]
wordToGroup {timeSig: sig} (RelativeWord notes duration) = do
    TimeRange _ _ start <- get
    put <<< mkTimeRange1 $ addDurationMod sig start duration
    pure [noteTreeToTimedGroup notes duration]
wordToGroup settings (CompleteWord start notes duration) = asserts *> res
    where
        asserts = lift $ validateStartTime settings start
        res = lift $ Left "" -- TODO
