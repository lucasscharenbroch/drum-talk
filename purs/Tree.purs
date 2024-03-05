module Tree where

import Prelude
import Data.Array

data Tree a = Leaf a
            | Internal (Array (Tree a))

instance Functor Tree where
    map f (Leaf x) = Leaf (f x)
    map f (Internal xs) = Internal (map ((<$>) f) xs)

flattenTree :: forall a. Tree a -> Array a
flattenTree (Leaf x) = [x]
flattenTree (Internal trees) = concat <<< map flattenTree $ trees
