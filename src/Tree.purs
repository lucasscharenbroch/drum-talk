module Tree where

import Prelude
import Data.Array

import Data.Show

data Tree a = Leaf a
            | Internal (Array (Tree a))

instance Show a => Show (Tree a) where
    show (Leaf x) = show x
    show (Internal xs) = show xs