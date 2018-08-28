module Indexation.Data
(
  Index(..),
  EntityTable,
  IndexTable,
  Indexer,
  IndexSet,
  lookupEntity,
  lookupIndex,
  createIndexSet,
  lookupInIndexSet,
)
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances ()
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector


lookupEntity :: Index entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? indexPrim

lookupIndex :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> Maybe (Index entity)
lookupIndex entity (IndexTable _ hashMap) =
  fmap Index (HashMap.lookup entity hashMap)

createIndexSet :: (Eq entity, Hashable entity, Foldable foldable) => IndexTable entity -> foldable entity -> IndexSet entity
createIndexSet (IndexTable size map) foldable = runST $ do
  mv <- MutableUnboxedVector.new size
  forM_ foldable $ \ entity -> forM_ (HashMap.lookup entity map) $ \ index -> do
    MutableUnboxedVector.unsafeWrite mv index True
  v <- UnboxedVector.unsafeFreeze mv
  return (IndexSet v)

lookupInIndexSet :: Index entity -> IndexSet entity -> Bool
lookupInIndexSet (Index indexInt) (IndexSet vec) = vec UnboxedVector.!? indexInt & fromMaybe False
