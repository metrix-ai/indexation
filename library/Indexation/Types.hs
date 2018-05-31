module Indexation.Types
where

import Indexation.Prelude


data IndexTable value = IndexTable {-# UNPACK #-} !Int {-# UNPACK #-} !(HashMap value Int)

newtype ValueTable value = ValueTable (Vector value)

newtype Index value = Index Int
