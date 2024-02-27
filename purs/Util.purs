module Util where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Array (foldM, fromFoldable, last, singleton)
import Data.List (List)
import Data.List.Types (NonEmptyList, toList)
import Data.String.CodeUnits (fromCharArray)
import Data.Rational(Rational, toNumber, (%))
import Data.Tuple (Tuple(..))
import Data.Int (floor)
import Data.Natural (Natural, intToNat)

-- Generic functions that may or may not be hidden under other names in libraries

scanlM :: forall m a b. Monad m => (b -> a -> m b) -> b -> Array a -> m (Array b)
scanlM f init = foldM f' []
    where f' bs a = ((<>) bs) <<< singleton <$> f (fromMaybe init (last bs)) a

fcompose :: forall f a b c. Functor f => (b -> c) -> (a -> f b) -> a -> f c
fcompose f g x = f <$> g x

infixr 9 fcompose as <.>

fmapflipped :: forall f a b. Functor f => f a -> (a -> b) -> f b
fmapflipped = flip (<$>)

infixl 1 fmapflipped as <&>

charListToStr :: List Char -> String
charListToStr = fromCharArray <<< fromFoldable

charNlistToStr :: NonEmptyList Char -> String
charNlistToStr = charListToStr <<< toList

ratToMixed :: Rational -> Tuple Int Rational
ratToMixed r = Tuple (floor n) (r - (floor n % 1))
    where n = toNumber r

id :: forall a. a -> a
id x = x

n0 = intToNat 0 :: Natural
n1 = intToNat 1 :: Natural
n2 = intToNat 2 :: Natural
n3 = intToNat 3 :: Natural
n4 = intToNat 4 :: Natural
