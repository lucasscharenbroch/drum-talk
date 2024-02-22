module Timing where

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Rational
import Data.Traversable
import Data.Tuple
import Prelude
import Tree
import Note
import Word
import Util

import Data.Array (foldl, zip, zipWith, drop, concat)
import Data.Int (ceil)
import Data.Int.Bits ((.&.))
import JS.BigInt (toInt)
import Parse (Settings, TimeSig(..))

data TimedGroup = TimedGroup (Tree WeightedNote) Duration
                | TimedNote Note Duration
                | TimedRest Duration

type TimeInfo =
    { start :: Time
    , defEnd :: Time
    }

timeify :: Tuple Settings (Array Word) -> Either String (Array TimedGroup)
timeify (Tuple settings words) = validateSettings settings *> res
    where
        zero = Time (0 % 1)
        zeroI = {start: zero, defEnd: zero}
        res = do
            times <- scanlM (wordToTime settings) zeroI words
            let timeDiff = zip times (drop 1 times <> [zeroI])
            pure <<< concat $ zipWith (calcDurationAndRests settings) timeDiff words

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

treeToTimedGroup :: Tree WeightedNote -> Duration -> TimedGroup
treeToTimedGroup (Leaf (WeightedNote note _)) duration = TimedNote note duration
treeToTimedGroup (Leaf (WeightedRest _)) duration = TimedRest duration
treeToTimedGroup tree@(Internal _) duration = TimedGroup tree duration

wordToTime :: Settings -> TimeInfo -> Word -> Either String TimeInfo
wordToTime settings _ (AbsoluteWord time _) = asserts *> Right res
    where
        asserts = validateStartTime settings time
        {defDuration, timeSig: sig} = settings
        res =
            { start: time
            , defEnd: addDurationMod sig time defDuration
            }
wordToTime {timeSig: sig} lastNoteTime (RelativeWord _ duration) = Right res
    where
        {defEnd: lastDefEnd} = lastNoteTime
        end = addDurationMod sig lastDefEnd duration
        res =
            { start: lastDefEnd
            , defEnd: end
            }
wordToTime settings _ (CompleteWord start _ duration) = asserts *> Right res
    where
        {timeSig: sig} = settings
        asserts = validateStartTime settings start
        end = addDurationMod sig start duration
        res =
            { start
            , defEnd: end
            }

calcDurationAndRests :: Settings -> Tuple TimeInfo TimeInfo -> Word -> Array TimedGroup
calcDurationAndRests _ _ (RelativeWord tree duration) = [treeToTimedGroup tree duration]
calcDurationAndRests _ _ (CompleteWord _ tree duration) = [treeToTimedGroup tree duration]
calcDurationAndRests settings (Tuple thisNoteTimeI nextNoteTimeI) (AbsoluteWord _ _) = res
    where
        {timeSig: sig, defNote, defDuration} = settings
        {start} = thisNoteTimeI
        {start: nextStart} = nextNoteTimeI
        inf = Duration (999 % 1)
        zero = Duration (0 % 1)
        toNextNote = subTimeMod sig nextStart start
        toNextBeat = case start of
            Time r -> ((flip (subTimeMod sig) $ start) <<< Time <<< fromInt <<< ceil <<< toNumber) $ r
        duration = foldl min inf [toNextNote, toNextBeat, defDuration]
        timedNote = TimedNote defNote duration
        res
            | toNextNote - duration > zero = [timedNote, TimedRest (toNextNote - duration)]
            | otherwise = [timedNote]
