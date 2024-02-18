module Tree where

import Prelude
import Data.Array

data Tree a = Leaf a
            | Internal (Array (Tree a))