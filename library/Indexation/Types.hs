module Indexation.Types
where

import Indexation.Prelude
import qualified Data.HashTable.IO as HashtablesIO


data Indexer entity = Indexer {-# UNPACK #-} !(IORef Int) {-# UNPACK #-} !(HashtablesIO.BasicHashTable entity Int)

data IndexTable entity = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap entity Int)

newtype EntityTable entity = EntityTable (Vector entity)

newtype Index entity = Index Int
