module Indexation.Cereal.Put
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize.Put
import qualified Data.Vector.Generic as B


{-# INLINE putVector #-}
putVector :: (B.Vector vector element) => Putter element -> Putter (vector element)
putVector putElement vector =
  putSize *> putElements
  where
    putSize = putInt64le (fromIntegral (B.length vector))
    putElements = B.foldM'_ (const putElement) () vector

putEntityTable :: Putter entity -> Putter (EntityTable entity)
putEntityTable putEntity (EntityTable vector) =
  putVector putEntity vector
