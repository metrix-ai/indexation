module Indexation.Utils.Unfoldr
where

import Indexation.Prelude
import DeferredFolds.Unfoldr
import qualified Data.HashMap.Strict as HashMap


hashMapAt :: (Hashable a, Eq a) => a -> HashMap a b -> Unfoldr b
hashMapAt a = foldable . HashMap.lookup a

hashMapValuesByKeys :: (Hashable a, Eq a, Foldable foldable) => foldable a -> HashMap a b -> Unfoldr b
hashMapValuesByKeys keys hashMap = do
  key <- foldable keys
  hashMapAt key hashMap
