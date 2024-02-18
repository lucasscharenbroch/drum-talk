module Util where

import Prelude

import Control.Monad.State.Trans
import Data.Traversable
import Data.Array
import Data.Maybe

-- Generic functions that may or may not be hidden under other names in libraries

scanlM :: forall m a b. Monad m => (b -> a -> m b) -> b -> Array a -> m (Array b)
scanlM f init = foldM f' []
    where f' bs a = ((<>) bs) <<< singleton <$> f (fromMaybe init (last bs)) a
