module Indexation.Constructors.EntityTable
where

import Indexation.Prelude hiding (lookup)
import Indexation.Types
import qualified Indexation.Utils.Vector as A


indexTable :: IndexTable entity -> EntityTable entity
indexTable (IndexTable size table) =
  EntityTable vector
  where
    vector =
      A.indexHashMapWithSize size table
