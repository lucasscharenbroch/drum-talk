module Tree where

import Prelude
import Data.Array

import Data.Show

data Tree a = Leaf a
            | Internal (Array (Tree a))

instance Functor Tree where
    map f (Leaf x) = Leaf (f x)
    map f (Internal xs) = Internal (map ((<$>) f) xs)

instance Show a => Show (Tree a) where
    show (Leaf x) = show x
    show (Internal xs) = show xs

flattenTree :: forall a. Tree a -> Array a
flattenTree (Leaf x) = [x]
flattenTree (Internal trees) = concat <<< map flattenTree $ trees
