module Indexation.Types
where

import Indexation.Prelude
import qualified StmContainers.Map as StmMap
import qualified Data.Vector.Unboxed as UnboxedVector


data Indexer entity = Indexer {-# UNPACK #-} !(TVar Int) {-# UNPACK #-} !(StmMap.Map entity Int)

data IndexTable entity = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap entity Int)

newtype EntityTable entity = EntityTable (Vector entity)

newtype Index entity = Index Int

{-|
Set of indices.
A more efficient alternative to @HashSet (Index entity)@.
-}
newtype IndexSet entity = IndexSet (UnboxedVector.Vector Bool)
