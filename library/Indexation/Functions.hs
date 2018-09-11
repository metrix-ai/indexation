module Indexation.Functions
where

import Indexation.Prelude
import Indexation.Types
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector
import qualified DeferredFolds.Unfoldr as Unfoldr
import qualified DenseIntSet


lookupEntity :: Index entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? indexPrim

lookupIndex :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> Maybe (Index entity)
lookupIndex entity (IndexTable _ hashMap) =
  fmap Index (HashMap.lookup entity hashMap)

createIndexSet :: (Eq entity, Hashable entity, Foldable foldable) => IndexTable entity -> foldable entity -> IndexSet entity
createIndexSet (IndexTable size map) entities =
  IndexSet (DenseIntSet.foldable size (Unfoldr.hashMapValues map (Unfoldr.foldable entities)))

lookupInIndexSet :: Index entity -> IndexSet entity -> Bool
lookupInIndexSet (Index indexInt) (IndexSet set) = DenseIntSet.lookup indexInt set

mergeIndexSets :: IndexSet entity -> IndexSet entity -> IndexSet entity
mergeIndexSets (IndexSet a) (IndexSet b) = IndexSet (DenseIntSet.intersection (DenseIntSet.compose a <> DenseIntSet.compose b))

topCountedIndexSet :: Int -> IndexCounts a -> IndexSet a
topCountedIndexSet amount (IndexCounts countVec) = IndexSet (DenseIntSet.topValueIndices compare amount countVec)

indexSetByMinCount :: Word32 -> IndexCounts a -> IndexSet a
indexSetByMinCount min (IndexCounts countVec) = IndexSet (DenseIntSet.filteredIndices (>= min) countVec)

countIndexSet :: IndexSet a -> Int
countIndexSet (IndexSet set) = DenseIntSet.size set

reindexationTable :: IndexSet a -> EntityTable (Index a)
reindexationTable (IndexSet set) = EntityTable (coerce (DenseIntSet.presentElementsVector set :: Vector Int))

reindex :: IndexSet a -> EntityTable a -> EntityTable a
reindex (IndexSet set) (EntityTable vec) = EntityTable (DenseIntSet.filterVector set vec)
