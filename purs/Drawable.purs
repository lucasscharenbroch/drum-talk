module Drawable where

import Data.Either
import Data.List
import Note
import Prelude
import Tree
import Util
import Word

import Data.Array (zipWith, filter, head, fromFoldable, length)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Natural (natToInt)
import Data.Rational (Rational, (%))
import Data.Traversable (sum, traverse)
import Debug (spy)
import Parse (Settings, TimeSig(..), sigDenom, sigToR)
import Timing (TimedGroup(..))

import Data.List as List

type DrawableMeasure = Array BeamedNotes
type BeamedNotes = Array DrawableNote

data DrawableNote = DrawableRest Duration
                  | DrawableNote Note Duration
                  | DrawableTuplet (Array DrawableNote) Duration Number Number

instance Show DrawableNote where
    show (DrawableRest d) = "(Drawable Rest " <> show d <> ")"
    show (DrawableNote n d) = "(Drawable note " <> show n <> " " <> show d <> ")"
    show (DrawableTuplet ns d x y) = "(Drawable tuplet " <> show ns <> " " <> show d <> " " <> show x <> ":" <> show y <> ")"

toDrawable :: Settings -> Array TimedGroup -> Either String (Array DrawableMeasure)
toDrawable settings = (pure <<< splitEvenTuplets)
                  >=> sectionIntoMeasures settings
                  >=> (pure <<< map (drawMeasure settings))

splitEvenTuplets :: Array TimedGroup -> Array TimedGroup
splitEvenTuplets = id -- TODO

getDuration :: TimedGroup -> Duration
getDuration (TimedGroup _ d) = d
getDuration (TimedNote _ d) = d
getDuration (TimedRest d) = d

sectionIntoMeasures :: Settings -> Array TimedGroup -> Either String (Array (Array TimedGroup))
sectionIntoMeasures settings timedGroups = iter [] [] (Duration $ 0 % 1) (List.fromFoldable timedGroups)
    where {timeSig} = settings
          measureDuration = Duration <<< sigToR $ timeSig
          iter :: (Array (Array TimedGroup)) -> (Array TimedGroup) -> Duration -> (List TimedGroup) -> Either String (Array (Array TimedGroup))
          iter accs acc sum (Cons x xs)
              | sum + getDuration x < measureDuration = iter accs (acc <> [x]) (sum + getDuration x) xs
              | sum + getDuration x == measureDuration = iter (accs <> [acc <> [x]]) [] (Duration (0 % 1)) xs
              | otherwise = case x of
                  (TimedNote n d) -> iter (accs <> [acc <> [x']]) [] (Duration $ 0 % 1) (x'' : xs)
                      where d' = measureDuration - sum
                            x' = TimedNote n d'
                            x'' = TimedRest (d - d')
                            _ = spy "(n) measure duration" measureDuration
                            _ = spy "(n) sum" sum
                            _ = spy "(n) getDuration" (getDuration x)
                  (TimedRest d) -> iter (accs <> [acc <> [x']]) [] (Duration $ 0 % 1) (x'' : xs)
                      where d' = measureDuration - sum
                            x' = TimedRest d'
                            x'' = TimedRest (d - d')
                            _ = spy "measure duration" measureDuration
                            _ = spy "sum" sum
                            _ = spy "getDuration" (getDuration x)
                  _ -> Left "Timed group (tuplet) spans measure" -- TODO try to split? (factor out)
          iter accs [] _ _ = Right accs
            where _ = spy "accs =>" accs
          iter a b c d = Left $ "Supplied rhythms don't evenly fill measures"
              where _ = spy "accs" a
                    _ = spy "acc" b
                    _ = spy "sum" c
                    _ = spy "xxs" d
          _ = spy "groups" timedGroups

beamify :: TimeSig -> Array DrawableNote -> Array BeamedNotes
beamify timeSig = fromFoldable <<< _beamify (Duration $ 1 % sigDenom timeSig) [] d0 <<< List.fromFoldable

_beamify :: Duration -> Array DrawableNote -> Duration -> List DrawableNote -> List BeamedNotes
_beamify beatNote acc d (Cons x xs) = case x of
    (DrawableRest _)
        | length acc == 0 -> [x] : _beamify beatNote [] (addDurMod d dur) xs
        | otherwise -> acc : [x] : _beamify beatNote [] (addDurMod d dur) xs
    _
        | d + dur == beatNote -> (acc <> [x]) : _beamify beatNote [] (addDurMod d dur) xs
        | d + dur < beatNote -> _beamify beatNote (acc <> [x]) (d + dur) xs
        | length acc == 0 -> [x] : _beamify beatNote [] (addDurMod d dur) xs
        | otherwise -> acc : _beamify beatNote [] (addDurMod d dur) (x : xs)
    where dur = case x of
                DrawableNote _ y -> y
                DrawableTuplet _ y _ _ -> y
                DrawableRest y -> y
          addDurMod a b
              | a + b >= beatNote = addDurMod a (b - beatNote)
              | otherwise = a + b
_beamify beatNote acc _ Nil
    | length acc == 0 = Nil
    | otherwise = acc : Nil

drawMeasure :: Settings -> Array TimedGroup -> DrawableMeasure
drawMeasure {timeSig} timedGroups = beamify timeSig <<< map groupToDrawableNote $ timedGroups
    where groupToDrawableNote :: TimedGroup -> DrawableNote
          groupToDrawableNote (TimedNote n d) = DrawableNote n d
          groupToDrawableNote (TimedRest d) = DrawableRest d
          groupToDrawableNote (TimedGroup tree duration) = treeToDrawable tree duration
          treeToDrawable :: Tree WeightedNote -> Duration -> DrawableNote
          treeToDrawable (Leaf (WeightedNote n _)) d = DrawableNote n d
          treeToDrawable (Leaf (WeightedRest _)) d = DrawableRest d
          treeToDrawable tree@(Internal ts) d = DrawableTuplet (zipWith treeToDrawable ts durs) d (toNumber totalWeight) (durationToNumber $ d / durUnit)
            where totalWeight = natToInt <<< sum <<< map getWeight <<< flattenTree $ tree
                  _durUnit = d * Duration (1 % totalWeight)
                  durUnit = fromMaybe d32 <<< head <<< filter ((<=) _durUnit) $ [d32, d16, d8, d4, d2, d1]
                  durs = map ((*) durUnit <<< Duration <<< (\i -> i % 1) <<< natToInt <<< sum <<< map getWeight <<< flattenTree) $ ts
