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
import qualified Indexation.Utils.Vector as Vector


indexTableEntityTable :: IndexTable entity -> EntityTable entity
indexTableEntityTable (IndexTable size table) =
  EntityTable (Vector.indexHashMapWithSize size table)

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

lookupNewIndex :: Index entity -> ReindexTable entity -> Maybe (Index entity)
lookupNewIndex (Index oldIndexInt) (ReindexTable mapVector) = fmap Index (join (mapVector Vector.!? oldIndexInt))

intersectIndexSets :: [IndexSet entity] -> IndexSet entity
intersectIndexSets = IndexSet . DenseIntSet.intersections . coerce

uniteIndexSets :: [IndexSet entity] -> IndexSet entity
uniteIndexSets = IndexSet . DenseIntSet.unions . coerce

invertIndexSet :: IndexSet entity -> IndexSet entity
invertIndexSet (IndexSet x) = IndexSet (DenseIntSet.invert x)

topCountedIndexSet :: Int -> IndexCounts a -> IndexSet a
topCountedIndexSet amount (IndexCounts countVec) = IndexSet (DenseIntSet.topValueIndices compare amount countVec)

indexSetByMinCount :: Word32 -> IndexCounts a -> IndexSet a
indexSetByMinCount min (IndexCounts countVec) = IndexSet (DenseIntSet.filteredIndices (>= min) countVec)

countIndexSet :: IndexSet a -> Int
countIndexSet (IndexSet set) = DenseIntSet.population set

newIndexToOldIndexTable :: IndexSet a -> EntityTable (Index a)
newIndexToOldIndexTable (IndexSet set) = EntityTable (DenseIntSet.presentElementsVector set Index)

oldIndexToNewIndexTable :: IndexSet a -> ReindexTable a
oldIndexToNewIndexTable (IndexSet set) = ReindexTable (DenseIntSet.indexVector set id)

filterEntityTable :: IndexSet a -> EntityTable a -> EntityTable a
filterEntityTable (IndexSet set) (EntityTable vec) = EntityTable (DenseIntSet.filterVector set vec)
