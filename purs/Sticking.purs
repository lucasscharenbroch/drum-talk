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

alternate :: Stick -> Stick
alternate StrongLeft = StrongRight
alternate StrongRight = StrongLeft
alternate WeakLeft = WeakRight
alternate WeakRight = WeakLeft

alternateSticking :: Array TimedGroup -> Array TimedGroup
alternateSticking = fromFoldable <<< _alternateSticking StrongLeft <<< List.fromFoldable

_alternateSticking :: Stick -> List TimedGroup -> List TimedGroup
_alternateSticking s (r@(TimedRest _):tgs) = r : _alternateSticking s tgs
_alternateSticking s (n@(TimedNote note dur):tgs)
    | isStrong note.stick = n : _alternateSticking note.stick tgs
    | otherwise = TimedNote (note {stick = alternate s}) dur : _alternateSticking (alternate s) tgs
_alternateSticking s ((TimedGroup tree dur):tgs) = TimedGroup t' dur : _alternateSticking s' tgs
    where {s: s', t: t'} = _alternateTree s false tree
_alternateSticking _ Nil = Nil

-- (f = should flip relative stickings?)
_alternateTree :: Stick -> Boolean -> Tree WeightedNote -> {s :: Stick, t :: Tree WeightedNote, f :: Boolean}
_alternateTree s f l@(Leaf (WeightedRest _)) = {s, t: l, f}
_alternateTree s f l@(Leaf (WeightedNote note nat))
    | isStrong note.stick = {s: note.stick, t: l, f: false}
    | isStrong s && (isRight s == isRight note.stick) = {s: note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), f: true}
    | f = {s: note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), f: true}
    | otherwise = {s: note.stick, t: l, f: false}
_alternateTree s f (Internal ts) = {s: resS, t: Internal resTs, f: resF}
    where rec :: Tree WeightedNote -> State (Tuple Stick Boolean) (Tree WeightedNote)
          rec x = do (Tuple _s _f) <- get
                     let {s: s', t: t', f: f'} = _alternateTree _s _f x
                     put (Tuple s' f')
                     pure t'
          (Tuple resTs (Tuple resS resF)) = runState (traverse rec ts) (Tuple s f)
