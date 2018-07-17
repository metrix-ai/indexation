module Indexation.Cereal.Put
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize.Put
import qualified Data.Vector as B


putVector :: Putter element -> Putter (Vector element)
putVector putElement vector =
  putSize *> putElements
  where
    putSize = putInt64le (fromIntegral (B.length vector))
    putElements = traverse_ putElement vector

putEntityTable :: Putter entity -> Putter (EntityTable entity)
putEntityTable putEntity (EntityTable vector) =
  putVector putEntity vector

putIndex :: Putter (Index entity)
putIndex (Index int) = putInt64le (fromIntegral int)
