module Timing where

import Prelude
import Words
import Data.Either
import Data.Tuple
import Data.Rational
import Data.Foldable
import Control.Monad.State.Trans
import Data.Traversable
import Data.Maybe
import JS.BigInt (toInt)

import Parse (Settings)
import Data.Int.Bits ((.&.))

data TimedGroup = TimedGroup (Array TimedGroup) Duration
                | TimedNote Note Duration

validateSettings :: Settings -> Either String Unit
validateSettings settings = result
    where
        sig = settings.timeSignature
        sigNum = fromMaybe (-1) <<< toInt <<< numerator $ sig :: Int
        sigDenom = fromMaybe (-1) <<< toInt <<< denominator $ sig :: Int
        isPow2 x = x .&. (x - 1) == 0
        pow2Err = "Time signature's denominator should be a power of 2"
        positiveErr = "Time signature should be positive"
        result
            | sigNum <= 0 || sigDenom <= 0 = Left positiveErr
            | isPow2 sigDenom = Left pow2Err
            | otherwise = Right unit

timeify :: Tuple Settings (Array Word) -> Either String (Array TimedGroup)
timeify (Tuple settings words) = validateSettings settings *> evalStateT (traverse timeStep words) zero
    where zero = Time (0 % 1)

timeStep :: Word -> StateT Time (Either String) TimedGroup
timeStep _ = lift (Left "x")

{-
data Word = AbsoluteWord Time -- implicit duration
          | RelativeWord (Array Note) Duration -- implicit time
          | CompleteWord Time (Array Note) Duration
          -}
