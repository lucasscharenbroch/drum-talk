module Timing where

import Data.Either
import Data.Foldable
import Data.Generic.Rep
import Data.Maybe
import Data.Rational
import Data.Show
import Data.Show.Generic
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
import Data.Natural (natToInt)
import Debug (spy)
import JS.BigInt (toInt)
import Parse (Settings, TimeSig(..), sigToR, sigDenom)

data TimedGroup = TimedGroup (Tree WeightedNote) Duration
                | TimedNote Note Duration
                | TimedRest Duration

derive instance genericTimedGroup :: Generic TimedGroup _
instance Show TimedGroup where
    show = genericShow

type TimeInfo =
    { start :: MeasureTime
    , earlyEnd :: MeasureTime
    , defEnd :: MeasureTime
    }

timeify :: Settings -> Array Word -> Either String (Array TimedGroup)
timeify settings words = validateSettings settings *> res
    where
        _ = spy "words" words
        zero = MeasureTime (0 % 1)
        zeroI = {start: zero, earlyEnd: zero, defEnd: zero}
        res = do
            times <- scanlM (wordToTime settings) zeroI words
            let timeDiff = zip times (drop 1 times <> [zeroI])
            pure <<< concat $ zipWith (calcDurationAndRests settings) timeDiff words

validateSettings :: Settings -> Either String Unit
validateSettings {timeSig: TimeSig sigNum sigDenom} = res
    where
        isPow2 x = x .&. (x - 1) == 0
        pow2Err = "Time signature (" <> show sigNum <> "/" <> show sigDenom <> ") should have a power-of-2 denominator"
        res
            | not $ isPow2 (natToInt sigDenom) = Left pow2Err
            | otherwise = Right unit

validateStartTime :: Settings -> Time -> Either String Unit
validateStartTime {timeSig: sig} (MeasureOffset t)
    | t >= sigToR sig = Left $ "Note start-time exceeds size of measure: " <> show t
    | otherwise = Right unit
validateStartTime _ (BeatOffset _) = Right unit -- beat offsets are hard-coded => in bounds

-- Calculates a new time given a start-time, duration, and signature
addDurationMod :: TimeSig -> MeasureTime -> Duration -> MeasureTime
addDurationMod sig (MeasureTime t) (Duration d)
    | t + d < sigToR sig = MeasureTime $ t + d
    | otherwise = MeasureTime $ t + d - (sigToR sig)

subTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> Duration
subTimeMod sig (MeasureTime t1) (MeasureTime t2)
    | t1 - t2 < (0 % 1) = Duration $ t1 - t2 + (sigToR sig)
    | otherwise = Duration $ t1 - t2

addTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> MeasureTime
addTimeMod sig (MeasureTime t1) (MeasureTime t2)
    | t1 + t2 >= sigToR sig = MeasureTime $ t1 + t2 - (sigToR sig)
    | otherwise = MeasureTime $ t1 + t2

treeToTimedGroup :: Tree WeightedNote -> Duration -> TimedGroup
treeToTimedGroup (Leaf (WeightedNote note _)) duration = TimedNote note duration
treeToTimedGroup (Leaf (WeightedRest _)) duration = TimedRest duration
treeToTimedGroup tree@(Internal _) duration = TimedGroup tree duration

timeToMeasureTime :: Settings -> TimeInfo -> Time -> MeasureTime
timeToMeasureTime _ _ (MeasureOffset m) = MeasureTime m
timeToMeasureTime s i (BeatOffset b) = result
    where {timeSig} = s
          {earlyEnd} = i
          denom = sigDenom timeSig
          b' = b * (1 % denom)
          MeasureTime earlyEndRat = earlyEnd
          Tuple whole frac = ratToMixed (earlyEndRat * (denom % 1))
          result = case compare b frac of
              EQ -> earlyEnd
              GT -> addTimeMod timeSig (MeasureTime $ whole % denom) (MeasureTime b')
              LT -> addTimeMod timeSig (MeasureTime $ (whole + 1) % denom) (MeasureTime b')

wordToTime :: Settings -> TimeInfo -> Word -> Either String TimeInfo
wordToTime settings lastTimeInfo (AbsoluteWord _time _) = asserts *> Right res
    where
        asserts = validateStartTime settings _time
        time = timeToMeasureTime settings lastTimeInfo _time
        _ = spy "" time
        {defDuration, minDuration, timeSig} = settings
        res =
            { start: time
            , earlyEnd: addDurationMod timeSig time minDuration
            , defEnd: addDurationMod timeSig time defDuration
            }
        _ = spy "" res
wordToTime {timeSig} lastTimeInfo (RelativeWord _ duration) = Right res
    where
        {defEnd: lastDefEnd} = lastTimeInfo
        end = addDurationMod timeSig lastDefEnd duration
        res =
            { start: lastDefEnd
            , earlyEnd: end
            , defEnd: end
            }
wordToTime settings lastTimeInfo (CompleteWord _start _ duration) = asserts *> Right res
    where
        {timeSig} = settings
        asserts = validateStartTime settings _start
        start = timeToMeasureTime settings lastTimeInfo _start
        end = addDurationMod timeSig start duration
        res =
            { start
            , earlyEnd: end
            , defEnd: end
            }

calcDurationAndRests :: Settings -> Tuple TimeInfo TimeInfo -> Word -> Array TimedGroup
calcDurationAndRests {timeSig} (Tuple thisTimeI nextTimeI) (RelativeWord tree duration) = res
    where toNextNote = subTimeMod timeSig nextTimeI.start thisTimeI.start
          restDuration = toNextNote - duration
          _res = [treeToTimedGroup tree duration]
          res
              | restDuration > d0 = _res <> [TimedRest restDuration]
              | otherwise = _res
calcDurationAndRests {timeSig} (Tuple thisTimeI nextTimeI) (CompleteWord _ tree duration) = res
    where toNextNote = subTimeMod timeSig nextTimeI.start thisTimeI.start
          restDuration = toNextNote - duration
          _res = [treeToTimedGroup tree duration]
          res
              | restDuration > d0 = _res <> [TimedRest restDuration]
              | otherwise = _res
calcDurationAndRests settings (Tuple thisTimeI nextTimeI) (AbsoluteWord _ _) = res
    where
        {timeSig, defNote, defDuration} = settings
        {start} = thisTimeI
        {start: nextStart} = nextTimeI
        inf = Duration (999 % 1)
        toNextNote = subTimeMod timeSig nextStart start
        _toNextBeat = case start of
            MeasureTime r -> ((flip (subTimeMod timeSig) $ start) <<< MeasureTime <<< fromInt <<< ceil <<< toNumber) $ r
        toNextBeat
            | _toNextBeat == Duration (0 % 1) = Duration (1 % 1)
            | otherwise = _toNextBeat
        _ = spy ">" [toNextNote, toNextBeat, defDuration]
        duration = foldl min inf [toNextNote, toNextBeat, defDuration]
        timedNote = TimedNote defNote duration
        res
            | toNextNote - duration > d0 = [timedNote, TimedRest (toNextNote - duration)]
            | otherwise = [timedNote]
