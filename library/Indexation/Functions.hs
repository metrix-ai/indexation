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
import qualified VectorExtras.Immutable as Vector


indexPrim :: Index prim a -> prim
indexPrim (Index x) = x

indexTableEntityTable :: Integral prim => IndexTable prim entity -> EntityTable entity
indexTableEntityTable (IndexTable size hashMap) =
  EntityTable (Vector.assocUnfoldrWithSize size (fmap (swap . fmap fromIntegral) (Unfoldr.hashMapAssocs hashMap)))

lookupEntity :: Integral prim => Index prim entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? fromIntegral indexPrim

lookupIndex :: (Eq entity, Hashable entity) => entity -> IndexTable prim entity -> Maybe (Index prim entity)
lookupIndex entity (IndexTable _ hashMap) =
  fmap Index (HashMap.lookup entity hashMap)

createIndexSet :: (Eq entity, Hashable entity, Foldable foldable, Integral prim) => IndexTable prim entity -> foldable entity -> IndexSet entity
createIndexSet (IndexTable size map) entities =
  IndexSet (DenseIntSet.foldable size (fmap fromIntegral (Unfoldr.hashMapValues map (Unfoldr.foldable entities))))

lookupInIndexSet :: Integral prim => Index prim entity -> IndexSet entity -> Bool
lookupInIndexSet (Index indexPrim) (IndexSet set) = DenseIntSet.lookup (fromIntegral indexPrim) set

lookupNewIndex :: Integral prim => Index prim entity -> ReindexTable prim entity -> Maybe (Index prim entity)
lookupNewIndex (Index oldIndexInt) (ReindexTable mapVector) = fmap (Index . fromIntegral) (join (mapVector Vector.!? fromIntegral oldIndexInt))

mergeIndexSets :: IndexSet entity -> IndexSet entity -> IndexSet entity
mergeIndexSets (IndexSet a) (IndexSet b) = IndexSet (DenseIntSet.intersection (DenseIntSet.compose a <> DenseIntSet.compose b))

topCountedIndexSet :: Int -> IndexCounts a -> IndexSet a
topCountedIndexSet amount (IndexCounts countVec) = IndexSet (DenseIntSet.topValueIndices compare amount countVec)

indexSetByMinCount :: Word32 -> IndexCounts a -> IndexSet a
indexSetByMinCount min (IndexCounts countVec) = IndexSet (DenseIntSet.filteredIndices (>= min) countVec)

countIndexSet :: IndexSet a -> Int
countIndexSet (IndexSet set) = DenseIntSet.size set

newIndexToOldIndexTable :: Num prim => IndexSet a -> EntityTable (Index prim a)
newIndexToOldIndexTable (IndexSet set) = EntityTable (DenseIntSet.presentElementsVector @Vector set (Index . fromIntegral))

oldIndexToNewIndexTable :: Num prim => IndexSet a -> ReindexTable prim a
oldIndexToNewIndexTable (IndexSet set) = ReindexTable (DenseIntSet.indexVector set fromIntegral)

filterEntityTable :: IndexSet a -> EntityTable a -> EntityTable a
filterEntityTable (IndexSet set) (EntityTable vec) = EntityTable (DenseIntSet.filterVector set vec)
