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
isRight WeakRight = true
isRight StrongRight = true
isRight _ = false

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
alternate NeutralStick = NeutralStick

stickNE :: Stick -> Stick -> Boolean
stickNE NeutralStick _ = false
stickNE _ NeutralStick = false
stickNE WeakLeft StrongLeft = false
stickNE StrongLeft WeakLeft = false
stickNE WeakRight StrongRight = false
stickNE StrongRight WeakRight = false
stickNE x y = x /= y

alternateSticking :: Array TimedGroup -> Array TimedGroup
alternateSticking = fromFoldable <<< _alternateSticking StrongLeft <<< List.fromFoldable

_alternateSticking :: Stick -> List TimedGroup -> List TimedGroup
_alternateSticking s (r@(TimedRest _):tgs) = r : _alternateSticking s tgs
_alternateSticking s (n@(TimedNote note dur):tgs)
    | isStrong note.stick = n : _alternateSticking note.stick tgs
    | otherwise = TimedNote (note {stick = alternate s}) dur : _alternateSticking (alternate s) tgs
_alternateSticking s ((TimedGroup tree dur):tgs) = TimedGroup t' dur : _alternateSticking s' tgs
    where {s: _s', t: t'} = _alternateTree s s tree
          s' = toStrong _s'
_alternateSticking _ Nil = Nil

-- (o = original value of previous stick)
_alternateTree :: Stick -> Stick -> Tree WeightedNote -> {s :: Stick, t :: Tree WeightedNote, o :: Stick}
_alternateTree s o l@(Leaf (WeightedRest _)) = {s, t: l, o}
_alternateTree s o l@(Leaf (WeightedNote note nat))
    | note.stick == NeutralStick = let s' = toStrong $ alternate s
                                   in {s: s', t: (Leaf (WeightedNote (note {stick = s'}) nat)), o: s'}
    | isStrong note.stick = {s: note.stick, t: l, o: note.stick}
    | isStrong o && (isRight s == isRight note.stick) = {s: alternate note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), o: note.stick}
    | isRight s /= isRight o = {s: alternate note.stick, t: (Leaf $ WeightedNote note {stick = alternate note.stick} nat), o: note.stick}
    | otherwise = {s: note.stick, t: l, o: note.stick}
_alternateTree s o (Internal ts) = {s: toStrong resS, t: Internal resTs, o: toStrong resS}
    where rec :: Tree WeightedNote -> State (Tuple Stick Stick) (Tree WeightedNote)
          rec x = do (Tuple _s _o) <- get
                     let {s: s', t: t', o: o'} = _alternateTree _s _o x
                     put (Tuple s' o')
                     pure t'
          (Tuple resTs (Tuple resS _)) = runState (traverse rec ts) (Tuple (toStrong s) (toStrong o))
