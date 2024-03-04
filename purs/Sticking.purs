module Sticking where

import Control.Monad.State
import Data.List
import Data.Traversable
import Data.Tuple
import Prelude
import Timing
import Tree
import Util

import Data.Array (fromFoldable)
import Data.List as List
import Debug (spy)
import Note (Stick(..), WeightedNote(..))

isLeft :: Stick -> Boolean
isLeft WeakLeft = true
isLeft StrongLeft = true
isLeft _ = false

isRight :: Stick -> Boolean
isRight = not <<< isLeft

isStrong :: Stick -> Boolean
isStrong StrongLeft = true
isStrong StrongRight = true
isStrong _ = false

toStrong :: Stick -> Stick
toStrong WeakLeft = StrongLeft
toStrong WeakRight = StrongRight
toStrong x = x

alternate :: Stick -> Stick
alternate StrongLeft = StrongRight
alternate StrongRight = StrongLeft
alternate WeakLeft = WeakRight
alternate WeakRight = WeakLeft

alternateSticking :: Array TimedGroup -> Array TimedGroup
alternateSticking = fromFoldable <<< _alternateSticking StrongLeft <<< List.fromFoldable

_alternateSticking :: Stick -> List TimedGroup -> List TimedGroup
_alternateSticking s (r@(TimedRest _):tgs) = r : _alternateSticking s tgs
    where _ = spy "s" []
_alternateSticking s (n@(TimedNote note dur):tgs)
    | isStrong note.stick = n : _alternateSticking note.stick tgs
    | otherwise = TimedNote (note {stick = alternate s}) dur : _alternateSticking (alternate s) tgs
    where _ = spy "s" [isStrong s && isLeft s, (not isStrong $ s) && isLeft s, (not isStrong $ s) && isRight s, isStrong s && isRight s]
          _ = spy "note" [isStrong ((note {stick = alternate s}).stick) && isLeft ((note {stick = alternate s}).stick), (not isStrong $ ((note {stick = alternate s}).stick)) && isLeft ((note {stick = alternate s}).stick), (not isStrong $ ((note {stick = alternate s}).stick)) && isRight ((note {stick = alternate s}).stick), isStrong ((note {stick = alternate s}).stick) && isRight ((note {stick = alternate s}).stick)]
_alternateSticking s ((TimedGroup tree dur):tgs) = TimedGroup t' dur : _alternateSticking s' tgs
    where {s: _s', t: t'} = _alternateTree s s tree
          s' = toStrong _s'
          _ = spy "s" [isStrong s && isLeft s, (not isStrong $ s) && isLeft s, (not isStrong $ s) && isRight s, isStrong s && isRight s]
          _ = spy "s'" [isStrong s' && isLeft s', (not isStrong $ s') && isLeft s', (not isStrong $ s') && isRight s', isStrong s' && isRight s']
_alternateSticking _ Nil = Nil

-- (o = original value of previous stick)
_alternateTree :: Stick -> Stick -> Tree WeightedNote -> {s :: Stick, t :: Tree WeightedNote, o :: Stick}
_alternateTree s o l@(Leaf (WeightedRest _)) = {s, t: l, o}
_alternateTree s o l@(Leaf (WeightedNote note nat))
    | isStrong note.stick = {s: note.stick, t: l, o: note.stick}
    | isStrong o && (isRight s == isRight note.stick) = {s: alternate note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), o: note.stick}
    | isRight s /= isRight o = {s: alternate note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), o: note.stick}
    | otherwise = {s: note.stick, t: l, o: note.stick}
_alternateTree s o (Internal ts) = {s: toStrong resS, t: Internal resTs, o: toStrong resO}
    where rec :: Tree WeightedNote -> State (Tuple Stick Stick) (Tree WeightedNote)
          rec x = do (Tuple _s _o) <- get
                     let {s: s', t: t', o: o'} = _alternateTree _s _o x
                     put (Tuple s' o')
                     pure t'
          (Tuple resTs (Tuple resS resO)) = runState (traverse rec ts) (Tuple (toStrong s) (toStrong o))
          _ = spy "resS" [resS == StrongLeft, resS == WeakLeft, resS == WeakRight, resS == StrongRight]
