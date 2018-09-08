module Indexation.Utils.Unfoldr
where

import Indexation.Prelude
import DeferredFolds.Unfoldr
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed.Bit as BitVec


hashMapAt :: (Hashable a, Eq a) => a -> HashMap a b -> Unfoldr b
hashMapAt a = foldable . HashMap.lookup a

hashMapValuesByKeys :: (Hashable a, Eq a, Foldable foldable) => foldable a -> HashMap a b -> Unfoldr b
hashMapValuesByKeys keys hashMap = do
  key <- foldable keys
  hashMapAt key hashMap

bitVecWords :: BitVector -> Unfoldr Word
bitVecWords vec = do
  index <- intsInRange 0 (pred (BitVec.wordLength vec))
  return (BitVec.indexWord vec index)
