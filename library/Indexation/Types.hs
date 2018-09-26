module Indexation.Types
where

import Indexation.Prelude
import qualified StmContainers.Map as StmMap
import qualified Data.Vector.Unboxed as UnboxedVector


data Indexer entity = Indexer {-# UNPACK #-} !(TVar Int) {-# UNPACK #-} !(StmMap.Map entity Int)

{-|
Index by entity table.
-}
data IndexTable prim entity = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap entity prim)

{-|
Map from old to new indices.
-}
newtype ReindexTable prim entity = ReindexTable (Vector (Maybe prim))

{-|
Entity by index table.
-}
newtype EntityTable entity = EntityTable (Vector entity)

{-|
A primitive wrapper for type-safe entity relations.
-}
newtype Index prim entity = Index prim

{-|
Set of indices.
A more efficient alternative to @HashSet (Index entity)@.
-}
newtype IndexSet entity = IndexSet DenseIntSet

newtype IndexCounts entity = IndexCounts (UnboxedVector.Vector Word32)
