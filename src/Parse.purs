module Parse where

import Data.List
import Data.Tuple
import Data.Either
import Data.Rational
import Prelude
import Words

newtype TimeSig = TimeSig Rational

type Settings =
    { timeSig :: TimeSig
    , defDuration :: Duration
    , minDuration :: Duration
    , maxDuration :: Duration
    , defNote :: Note
    }

parse :: String -> Either String (Tuple Settings (Array Word))
parse _ = Left "" -- TODO
