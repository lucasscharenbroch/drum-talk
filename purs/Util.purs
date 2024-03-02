module Util where

import Prelude

import Data.Array (foldM, fromFoldable, last, singleton, filter)
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.List.Types (NonEmptyList, toList)
import Data.Maybe (fromMaybe)
import Data.Natural (Natural, intToNat, natToInt)
import Data.Rational (Rational, denominator, toNumber, (%))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
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

type UnpackedDuration =
    { mainDuration :: String
    , numDots :: Number
    , tied :: Array String -- durations of trailing tied notes
    }

unpackDuration :: Duration -> UnpackedDuration
unpackDuration (Duration r) = res
    where coreDurs = [1 % 32, 1 % 16, 1 % 8, 1 % 4, 1 % 2, 1 % 1]
          biggestCoreLe x = fromMaybe (1 % 32) <<< last $ filter ((>=) x) coreDurs
          coreDurToStr = show <<< denominator
          mainDuration = biggestCoreLe r
          calcNumDots toGo doubleDotDur = _res
              where dotDur = doubleDotDur * (1 % 2)
                    _res
                        | dotDur < (1 % 32) = 0
                        | toGo < dotDur = 0
                        | otherwise = 1 + calcNumDots (toGo - dotDur) dotDur
          numDots = calcNumDots (r - mainDuration) mainDuration
          calcDotDuration 0 _ = 0 % 1
          calcDotDuration n d = (d * (1 % 2)) + calcDotDuration (n - 1) (d * (1 % 2))
          calcTied toGo
              | toGo <= (0 % 0) = []
              | otherwise = let d = biggestCoreLe toGo
                            in [d] <> calcTied (toGo - d)
          tied = calcTied (r - mainDuration - calcDotDuration numDots mainDuration)
          res =
              { mainDuration: coreDurToStr mainDuration
              , numDots: Int.toNumber numDots
              , tied: map coreDurToStr $ tied
              }

durationToNumber :: Duration -> Number
durationToNumber (Duration r) = toNumber r
