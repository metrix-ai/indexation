module Indexation.Types
where

import Indexation.Prelude
import qualified STMContainers.Map as A


data Indexer entity = Indexer (TVar Int) (A.Map entity Int)

data IndexTable entity = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap entity Int)

newtype EntityTable entity = EntityTable (Vector entity)

newtype Index entity = Index Int
