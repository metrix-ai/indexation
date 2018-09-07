module Indexation.Functions
where

import Indexation.Prelude
import Indexation.Types
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector
import qualified Data.Vector.Algorithms.Intro as IntroVectorAlgorithm
import qualified Indexation.Utils.UnboxedVector as UnboxedVector
import qualified Indexation.Utils.Unfoldr as Unfoldr


lookupEntity :: Index entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? indexPrim

lookupIndex :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> Maybe (Index entity)
lookupIndex entity (IndexTable _ hashMap) =
  fmap Index (HashMap.lookup entity hashMap)

createIndexSet :: (Eq entity, Hashable entity, Foldable foldable) => IndexTable entity -> foldable entity -> IndexSet entity
createIndexSet (IndexTable size map) entities =
  IndexSet (UnboxedVector.filledIndicesFoldable size (Unfoldr.hashMapValuesByKeys entities map))

lookupInIndexSet :: Index entity -> IndexSet entity -> Bool
lookupInIndexSet (Index indexInt) (IndexSet vec) = vec UnboxedVector.!? indexInt & fromMaybe False

mergeIndexSets :: IndexSet entity -> IndexSet entity -> IndexSet entity
mergeIndexSets (IndexSet vec1) (IndexSet vec2) = IndexSet $ UnboxedVector.zipWith (||) vec1 vec2

topCountedIndexSet :: Int -> IndexCounts a -> IndexSet a
topCountedIndexSet amount (IndexCounts countVec) = let
  countVecLength = UnboxedVector.length countVec
  limitedAmount = min amount countVecLength
  in runST $ do
    pairMVec <- UnboxedVector.unsafeThaw (UnboxedVector.imap (\ index count -> (count, index)) countVec)
    IntroVectorAlgorithm.selectBy (\ a b -> compare (fst b) (fst a)) pairMVec limitedAmount
    indexSetMVec <- MutableUnboxedVector.new countVecLength
    forM_ [0..(pred limitedAmount)] $ \ pairIndex -> do
      (_, index) <- MutableUnboxedVector.unsafeRead pairMVec pairIndex
      MutableUnboxedVector.write indexSetMVec index True
    IndexSet <$> UnboxedVector.unsafeFreeze indexSetMVec

indexSetByMinCount :: Word32 -> IndexCounts a -> IndexSet a
indexSetByMinCount min (IndexCounts countVec) = IndexSet (UnboxedVector.map (>= min) countVec)
