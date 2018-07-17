module Indexation.Folds
where

import Indexation.Prelude
import Indexation.Types
import Control.Foldl
import qualified Data.HashMap.Strict as B


indexTable :: (Eq entity, Hashable entity) => Fold entity (IndexTable entity)
indexTable =
  Fold step init extract
  where
    init = IndexTable 0 B.empty
    step (IndexTable index map) key =
      case B.lookup key map of
        Just _ -> IndexTable index map
        Nothing -> let
          newMap = B.insert key index map
          newIndex = succ index
          in IndexTable newIndex newMap
    extract = id
