module Indexation.Functions
where

import Indexation.Prelude
import Indexation.Types
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector
import qualified Data.Vector.Algorithms.Intro as IntroVectorAlgorithm
import qualified Data.Bit as Bit
import qualified Data.Vector.Unboxed.Bit as BitVec
import qualified Indexation.Utils.BitVector as UnboxedVector
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
lookupInIndexSet (Index indexInt) (IndexSet vec) = vec UnboxedVector.!? indexInt & maybe False Bit.toBool

mergeIndexSets :: IndexSet entity -> IndexSet entity -> IndexSet entity
mergeIndexSets (IndexSet a) (IndexSet b) = IndexSet (BitVec.intersection a b)

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
      MutableUnboxedVector.write indexSetMVec index (Bit.fromBool True)
    IndexSet <$> UnboxedVector.unsafeFreeze indexSetMVec

indexSetByMinCount :: Word32 -> IndexCounts a -> IndexSet a
indexSetByMinCount min (IndexCounts countVec) = IndexSet (UnboxedVector.map (Bit.fromBool . (>= min)) countVec)
