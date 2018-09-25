module Indexation.Potoki.Produce
(
  entities,
  counts,
  indexedCounts,
  indices,
)
where

import Indexation.Prelude
import Indexation.Types
import qualified Potoki.Produce as Produce
import qualified Data.Vector.Generic as GenericVector
import qualified DenseIntSet


entities :: EntityTable a -> Produce a
entities (EntityTable vector) = Produce.vector vector

{-|
Counts in the index-order.
-}
counts :: IndexCounts a -> Produce Word32
counts (IndexCounts vector) = Produce.vector vector

{-|
Counts in the index-order paired with their indices.
-}
indexedCounts :: IndexCounts a -> Produce (Int, Word32)
indexedCounts (IndexCounts vector) = Produce.vectorWithIndices vector

{-|
Indices, which are present in the set.
-}
indices :: IndexSet a -> Produce (Index a)
indices (IndexSet set) = coerce (Produce.unfoldr (DenseIntSet.presentElementsUnfoldr set))
