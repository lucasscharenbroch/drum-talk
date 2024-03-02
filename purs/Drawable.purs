module Drawable where

import Data.Either
import Data.List
import Note
import Prelude
import Util
import Word
import Tree

import Data.Rational ((%))
import Data.Traversable (sum, traverse)
import Parse (Settings, sigToR)
import Timing (TimedGroup(..))
import Data.Natural(natToInt)
import Data.Array(zipWith, filter, head)
import Data.Maybe (fromMaybe)

import Debug (spy)

type DrawableMeasure = Array DrawableNote

data DrawableNote = DrawableRest Duration
                  | DrawableNote Note Duration
                  | DrawableTuplet (Array DrawableNote) Duration

instance Show DrawableNote where
    show (DrawableRest d) = "(Drawable Rest " <> show d <> ")"
    show (DrawableNote n d) = "(Drawable note " <> show n <> " " <> show d <> ")"
    show (DrawableTuplet ns d) = "(Drawable tuplet " <> show ns <> " " <> show d <> ")"

toDrawable :: Settings -> Array TimedGroup -> Either String (Array DrawableMeasure)
toDrawable settings = (pure <<< splitEvenTuplets)
                  >=> sectionIntoMeasures settings
                  >=> (pure <<< map drawMeasure)

splitEvenTuplets :: Array TimedGroup -> Array TimedGroup
splitEvenTuplets = id -- TODO

getDuration :: TimedGroup -> Duration
getDuration (TimedGroup _ d) = d
getDuration (TimedNote _ d) = d
getDuration (TimedRest d) = d

sectionIntoMeasures :: Settings -> Array TimedGroup -> Either String (Array (Array TimedGroup))
sectionIntoMeasures settings timedGroups = iter [] [] (Duration $ 0 % 1) (fromFoldable timedGroups)
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

drawMeasure :: Array TimedGroup -> DrawableMeasure
drawMeasure timedGroups = map groupToDrawableNote timedGroups
    where groupToDrawableNote :: TimedGroup -> DrawableNote
          groupToDrawableNote (TimedNote n d) = DrawableNote n d
          groupToDrawableNote (TimedRest d) = DrawableRest d
          groupToDrawableNote (TimedGroup tree duration) = treeToDrawable tree duration
          treeToDrawable :: Tree WeightedNote -> Duration -> DrawableNote
          treeToDrawable (Leaf (WeightedNote n _)) d = DrawableNote n d
          treeToDrawable (Leaf (WeightedRest _)) d = DrawableRest d
          treeToDrawable tree@(Internal ts) d = DrawableTuplet (zipWith treeToDrawable ts durs) d
            where _durUnit = d * Duration (1 % (natToInt <<< sum <<< map getWeight $ flattenTree tree))
                  durUnit = fromMaybe d32 <<< head <<< filter ((<) _durUnit) $ [d32, d16, d8, d4, d2, d1]
                  durs = map ((*) durUnit <<< Duration <<< (\i -> i % 1) <<< natToInt <<< sum <<< map getWeight <<< flattenTree) $ ts
