module Indexation.Types
where

import Indexation.Prelude


data IndexTable entity = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap entity Int)

newtype EntityTable entity = EntityTable (Vector entity)

newtype Index entity = Index Int
