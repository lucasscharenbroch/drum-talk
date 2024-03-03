module Timing where

import Data.Either
import Data.Foldable
import Data.Generic.Rep
import Data.List
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
import Data.Array (concat, drop, foldl, fromFoldable, tail, zip, zipWith, length, head)
import Data.Int (ceil)
import Data.Int.Bits (xor, (.&.))
import Data.List as List
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
        {timeSig} = settings
        wholeWord = RelativeWord (Leaf $ WeightedRest n1) <<< Duration $ sigToR timeSig -- dummy leading whole note
        words' = [wholeWord] <> words
        res = do
            times <- scanlM (wordToTime settings) zeroI words'
            let timeDiff = zip times (drop 1 times <> [zeroI])
            pure <<< splitEvenTuplets <<< fromMaybe [] <<< tail <<< concat $ zipWith (calcDurationAndRests settings) timeDiff words'

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

subDurationMod :: TimeSig -> Duration -> Duration -> Duration
subDurationMod sig (Duration d1) (Duration d2)
    | d1 - d2 < (0 % 1) = Duration $ d1 - d2 + sigToR sig
    | otherwise = Duration $ d1 - d2

subTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> Duration
subTimeMod sig (MeasureTime t1) (MeasureTime t2)
    | t1 - t2 < (0 % 1) = Duration $ t1 - t2 + (sigToR sig)
    | otherwise = Duration $ t1 - t2

addTimeMod :: TimeSig -> MeasureTime -> MeasureTime -> MeasureTime
addTimeMod sig (MeasureTime t1) (MeasureTime t2)
    | t1 + t2 >= sigToR sig = MeasureTime $ t1 + t2 - (sigToR sig)
    | otherwise = MeasureTime $ t1 + t2

ratToNextBeat :: TimeSig -> Rational -> MeasureTime
ratToNextBeat timeSig = MeasureTime
                    <<< ((*) (1 % beatNote))
                    <<< fromInt <<< ceil <<< toNumber
                    <<< ((*) (beatNote % 1))
    where beatNote = sigDenom timeSig

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
        _ = spy "time" time
        {defDuration, minDuration, timeSig} = settings
        res =
            { start: time
            , earlyEnd: addDurationMod timeSig time minDuration
            , defEnd: addDurationMod timeSig time defDuration
            }
        _ = spy "res" res
wordToTime {timeSig} lastTimeInfo (RelativeWord _ duration) = Right res
    where
        {defEnd: lastDefEnd, earlyEnd} = lastTimeInfo
        -- end = addDurationMod timeSig lastDefEnd duration
        MeasureTime earlyEndR  = earlyEnd
        nextBeat = ratToNextBeat timeSig $ earlyEndR
        minusMod x y = subTimeMod timeSig x y
        nextBeatBetweenEnds = (nextBeat `minusMod` earlyEnd) + (lastDefEnd `minusMod` nextBeat) < d1
        start = if nextBeatBetweenEnds
                then nextBeat
                else lastDefEnd
        end = addDurationMod timeSig start duration
        res =
            { start
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
          restDuration = subDurationMod timeSig toNextNote duration
          _res = [treeToTimedGroup tree duration]
          res
              | restDuration > d0 = _res <> [TimedRest restDuration]
              | otherwise = _res
calcDurationAndRests {timeSig} (Tuple thisTimeI nextTimeI) (CompleteWord _ tree duration) = res
    where toNextNote = subTimeMod timeSig nextTimeI.start thisTimeI.start
          restDuration = subDurationMod timeSig toNextNote duration
          _res = [treeToTimedGroup tree duration]
          res
              | restDuration > d0 = _res <> [TimedRest restDuration]
              | otherwise = _res
calcDurationAndRests settings (Tuple thisTimeI nextTimeI) (AbsoluteWord _ note) = res
    where
        {timeSig, defDuration} = settings
        {start} = thisTimeI
        {start: nextStart} = nextTimeI
        (MeasureTime startR) = start
        inf = Duration (999 % 1)
        zeroToOne x
            | x == Duration (0 % 1) = Duration (1 % 1)
            | otherwise = x
        toNextNote = zeroToOne $ subTimeMod timeSig nextStart start
        toNextBeat = zeroToOne <<< (flip (subTimeMod timeSig) $ start) <<< ratToNextBeat timeSig $ startR
        _ = spy ">" [toNextNote, toNextBeat, defDuration]
        duration = foldl min inf [toNextNote, toNextBeat, defDuration]
        timedNote = TimedNote note duration
        res
            | toNextNote - duration > d0 = [timedNote, TimedRest (toNextNote - duration)]
            | otherwise = [timedNote]

splitEvenTuplets :: Array TimedGroup -> Array TimedGroup
splitEvenTuplets = fromFoldable <<< _splitEvenTuplets <<< List.fromFoldable

_splitEvenTuplets :: List TimedGroup -> List TimedGroup
_splitEvenTuplets Nil = Nil
_splitEvenTuplets (Cons tg@(TimedGroup tree duration) xs) = res <> _splitEvenTuplets xs
    where sumTree t = sum <<< map getWeight <<< flattenTree $ t
          split :: List (Tree WeightedNote) -> Maybe (Tuple (Array (Tree WeightedNote)) (Array (Tree WeightedNote)))
          split ts = _res
              where weights = map (natToInt <<< sumTree) $ ts
                    prefix = scanl (+) 0 $ weights
                    netSum = sum weights
                    {init: _before, rest: _after} = span ((>=) (netSum `div` 2) <<< snd) $ List.zip ts prefix
                    before = map fst _before
                    after = map fst _after
                    _res
                        | netSum `mod` 2 /= 0 = Nothing
                        | not ((netSum `div` 2) `elem` prefix) = Nothing
                        | otherwise = Just $ Tuple (fromFoldable before) (fromFoldable after)
                    _ = spy "prefix sum" prefix
                    _ = spy "net" netSum
                    _ = spy "before, after" (Tuple before after)
          rec ts = _splitEvenTuplets $ (TimedGroup (Internal ts) (duration * Duration (1 % 2))) : Nil
          res = case tree of
                (Leaf _) -> tg : Nil
                (Internal arr)
                    | length arr == 1 -> case head arr of
                                         Nothing -> TimedRest duration : Nil -- shouldn't happen
                                         (Just t) -> _splitEvenTuplets $ (TimedGroup t duration) : Nil
                (Internal arr) -> case split (List.fromFoldable arr) of
                                  Nothing -> tg : Nil
                                  Just (Tuple before after) -> rec before <> rec after
          _ = spy "splitting up" tg
          _ = spy "==>" (List.head res)
_splitEvenTuplets (Cons x xs) = x : _splitEvenTuplets xs
    where _ = spy "not splitting" x
