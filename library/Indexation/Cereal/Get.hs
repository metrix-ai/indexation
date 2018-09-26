module Indexation.Cereal.Get
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize.Get
import qualified Data.HashMap.Strict as A
import qualified Data.Vector.Generic as B


getIndexHashMap :: (Eq k, Hashable k) => Get k -> Get (HashMap k Int)
getIndexHashMap getKey =
  getSize >>= getAssociations
  where
    getSize = getInt64le
    getAssociations size = foldM step A.empty (enumFromTo 0 (pred (fromIntegral size)))
      where
        step hashMap index = do
          key <- getKey
          return (A.insert key index hashMap)

getIndexTableAsEntityTable :: (Eq entity, Hashable entity, Num prim, Enum prim) => Get entity -> Get (IndexTable prim entity)
getIndexTableAsEntityTable getEntity =
  do
    size <- getSize
    associations <- getAssociations size
    return (IndexTable size associations)
  where
    getSize = fromIntegral <$> getInt64le
    getAssociations size = foldM step A.empty (enumFromTo 0 (fromIntegral (pred size)))
      where
        step hashMap index = do
          entity <- getEntity
          return (A.insert entity index hashMap)

{-# INLINE getVector #-}
getVector :: B.Vector vector element => Get element -> Get (vector element)
getVector getElement =
  getSize >>= getElements
  where
    getSize = getInt64le
    getElements size = B.replicateM (fromIntegral size) getElement

getEntityTable :: Get entity -> Get (EntityTable entity)
getEntityTable getEntity =
  EntityTable <$> getVector getEntity
