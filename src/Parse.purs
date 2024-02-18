module Parse where

import Data.List
import Data.Tuple
import Data.Either
import Data.Rational
import Prelude
import Words

type Settings =
    { timeSignature :: Rational
    }

parse :: String -> Either String (Tuple Settings (Array Word))
parse _ = Left "" -- TODO
