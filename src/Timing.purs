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
import Util

import Data.Int.Bits ((.&.))
import JS.BigInt (toInt)
import Parse (Settings, TimeSig(..))

data TimedGroup = TimedGroup (Tree WeightedNote) Duration
                | TimedNote Note Duration
                | TimedRest Duration

timeify :: Tuple Settings (Array Word) -> Either String (Array TimedGroup)
timeify (Tuple settings words) = validateSettings settings *> res
    where
        zero = Tuple (Time (0 % 1)) (Time (0 % 1))
        res = do
            times <- scanlM wordToTime zero words
            let timeDiff = zip times (drop 1 times <> [Time (0 % 1)])
            pure <<< concat $ zipWith calcDurationAndRests timeDiff words

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

mkDefTimeRange :: Settings -> Time -> Tuple Time Time
mkDefTimeRange {minDuration, defDuration, timeSig: sig} time = Tuple early default
    where early = addDurationMod sig time minDuration
          default = addDurationMod sig time defDuration

wordToTime :: Settings -> Tuple Time Time -> Word -> (Either String) (Tuple Time Time)
wordToTime = ?todo -- TODO
{-
wordToTime settings (AbsoluteWord time) = asserts *> put newRange *> pure time
    where
        asserts = lift $ validateStartTime settings time
        {timeSig: sig, minDuration, defDuration, maxDuration, defNote} = settings
        newRange = mkDefTimeRange settings time
wordToTime {timeSig: sig} (RelativeWord notes duration) = do
    TimeRange _ _ start <- get
    put <<< mkTimeRange1 $ addDurationMod sig start duration
    pure $ noteTreeToTimedGroup notes duration
wordToTime settings (CompleteWord start notes duration) = asserts *> res
    where
        asserts = lift $ validateStartTime settings start
        res = lift $ Left "" -- TODO
        -}

calcDurationAndRests :: Tuple Time Time -> Word -> Array TimedGroup
calcDurationAndRests = ?todo -- TODO
