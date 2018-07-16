module Indexation.Cereal.Put
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize.Put
import qualified Data.HashMap.Strict as A
import qualified Indexation.HashMap as A
import qualified Data.Vector as B


putHashMapWithSize :: Putter k -> Putter v -> Putter (HashMap k v)
putHashMapWithSize keyPutter valuePutter hashMap =
  size *> associations
  where
    size = putInt64le (fromIntegral (A.size hashMap))
    associations = A.traverse_ association hashMap
    association key value = keyPutter key *> valuePutter value

putVector :: Putter element -> Putter (Vector element)
putVector putElement vector =
  putSize *> putElements
  where
    putSize = putInt64le (fromIntegral (B.length vector))
    putElements = traverse_ putElement vector

{-|
It's recommended to use 'putIndexTableAsEntityTable' instead.
The hashmap traversal implementation is inefficient and
the representation of 'EntityTable' is more compact.
-}
putIndexTableDirectly :: Putter entity -> Putter (IndexTable entity)
putIndexTableDirectly putEntity (IndexTable size hashMap) =
  putSize *> putAssociations
  where
    putSize = putInt64le (fromIntegral size)
    putAssociations = A.traverse_ putAssociation hashMap
    putAssociation key entity = putEntity key *> putInt64le (fromIntegral entity)

putEntityTable :: Putter entity -> Putter (EntityTable entity)
putEntityTable putEntity (EntityTable vector) =
  putVector putEntity vector
