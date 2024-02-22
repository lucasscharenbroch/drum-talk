module Timing where

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Rational
import Data.Traversable
import Data.Tuple
import Note
import Prelude
import Tree
import Util
import Word

import Control.Monad.Gen (resize)
import Data.Array (foldl, zip, zipWith, drop, concat)
import Data.Int (ceil)
import Data.Int.Bits ((.&.))
import JS.BigInt (toInt)
import Parse (Settings, TimeSig(..))

data TimedGroup = TimedGroup (Tree WeightedNote) Duration
                | TimedNote Note Duration
                | TimedRest Duration

type TimeInfo =
    { start :: MeasureTime
    , earlyEnd :: MeasureTime
    , defEnd :: MeasureTime
    }

timeify :: Tuple Settings (Array Word) -> Either String (Array TimedGroup)
timeify (Tuple settings words) = validateSettings settings *> res
    where
        zero = MeasureTime (0 % 1)
        zeroI = {start: zero, earlyEnd: zero, defEnd: zero}
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
validateStartTime {timeSig: TimeSig sig} (MeasureOffset t)
    | t >= sig = Left $ "Note start-time exceeds size of measure: " <> show t
    | otherwise = Right unit
validateStartTime _ (BeatOffset _) = Right unit -- beat offsets are hard-coded => in bounds

-- Calculates a new time given a start-time, duration, and signature
addDurationMod :: TimeSig -> MeasureTime -> Duration -> MeasureTime
addDurationMod (TimeSig sig) (MeasureTime t) (Duration d)
    | t + d < sig = MeasureTime $ t + d
    | otherwise = MeasureTime $ t + d - sig

subTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> Duration
subTimeMod (TimeSig sig) (MeasureTime t1) (MeasureTime t2)
    | t1 - t2 < (0 % 1) = Duration $ t1 - t2 + sig
    | otherwise = Duration $ t1 - t2

addTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> MeasureTime
addTimeMod (TimeSig sig) (MeasureTime t1) (MeasureTime t2)
    | t1 + t2 >= sig = MeasureTime $ t1 + t2 - sig
    | otherwise = MeasureTime $ t1 + t2

treeToTimedGroup :: Tree WeightedNote -> Duration -> TimedGroup
treeToTimedGroup (Leaf (WeightedNote note _)) duration = TimedNote note duration
treeToTimedGroup (Leaf (WeightedRest _)) duration = TimedRest duration
treeToTimedGroup tree@(Internal _) duration = TimedGroup tree duration

timeToMeasureTime :: Settings -> TimeInfo -> Time -> MeasureTime
timeToMeasureTime _ _ (MeasureOffset m) = MeasureTime m
timeToMeasureTime s i (BeatOffset b) = result
    where {timeSig: sig} = s
          {earlyEnd} = i
          MeasureTime earlyEndRat = earlyEnd
          Tuple whole frac = ratToMixed earlyEndRat
          result = case compare b frac of
              EQ -> earlyEnd
              LT -> addTimeMod sig (MeasureTime (whole % 1)) (MeasureTime b)
              GT -> addTimeMod sig (MeasureTime (whole % 1)) (MeasureTime (b + (1 % 1)))

wordToTime :: Settings -> TimeInfo -> Word -> Either String TimeInfo
wordToTime settings lastTimeInfo (AbsoluteWord _time _) = asserts *> Right res
    where
        asserts = validateStartTime settings _time
        time = timeToMeasureTime settings lastTimeInfo _time
        {defDuration, minDuration, timeSig: sig} = settings
        res =
            { start: time
            , earlyEnd: addDurationMod sig time minDuration
            , defEnd: addDurationMod sig time defDuration
            }
wordToTime {timeSig: sig} lastTimeInfo (RelativeWord _ duration) = Right res
    where
        {defEnd: lastDefEnd} = lastTimeInfo
        end = addDurationMod sig lastDefEnd duration
        res =
            { start: lastDefEnd
            , earlyEnd: end
            , defEnd: end
            }
wordToTime settings lastTimeInfo (CompleteWord _start _ duration) = asserts *> Right res
    where
        {timeSig: sig} = settings
        asserts = validateStartTime settings _start
        start = timeToMeasureTime settings lastTimeInfo _start
        end = addDurationMod sig start duration
        res =
            { start
            , earlyEnd: end
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
            MeasureTime r -> ((flip (subTimeMod sig) $ start) <<< MeasureTime <<< fromInt <<< ceil <<< toNumber) $ r
        duration = foldl min inf [toNextNote, toNextBeat, defDuration]
        timedNote = TimedNote defNote duration
        res
            | toNextNote - duration > zero = [timedNote, TimedRest (toNextNote - duration)]
            | otherwise = [timedNote]
