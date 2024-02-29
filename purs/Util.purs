module Util where

import Prelude

import Data.Int as Int

import Data.Maybe (fromMaybe)
import Data.Array (foldM, fromFoldable, last, singleton)
import Data.List (List)
import Data.List.Types (NonEmptyList, toList)
import Data.String.CodeUnits (fromCharArray)
import Data.Rational(Rational, toNumber, (%))
import Data.Tuple (Tuple(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.Either(Either(..))
import Word (Duration(..))

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
ratToMixed r = Tuple (Int.floor n) (r - (Int.floor n % 1))
    where n = toNumber r

id :: forall a. a -> a
id x = x

n0 = intToNat 0 :: Natural
n1 = intToNat 1 :: Natural
n2 = intToNat 2 :: Natural
n3 = intToNat 3 :: Natural
n4 = intToNat 4 :: Natural

-- Helpers for TS "bridge"

isRight :: forall a b. Either a b -> Boolean
isRight e = case e of
    Left _ -> false
    Right _ -> true

natToNum :: Natural -> Number
natToNum = Int.toNumber <<< natToInt

durationToString :: Duration -> String
durationToString (Duration r)
    | r == (1 % 32) = "32"
    | r == (1 % 16) = "16"
    | r == (1 % 8) = "8"
    | r == (1 % 4) = "4"
    | r == (1 % 2) = "2"
    | r == (1 % 1) = "1"
    | otherwise = "bad duration"
