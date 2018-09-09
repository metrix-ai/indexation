module Indexation.Potoki.Produce
(
  entities,
  counts,
  indexedCounts,
)
where

import Indexation.Prelude
import Indexation.Types
import qualified Potoki.Produce as Produce
import qualified Data.Vector.Generic as GenericVector


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
