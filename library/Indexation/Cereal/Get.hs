module Indexation.Cereal.Get
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize.Get
import qualified Data.HashMap.Strict as A
import qualified Indexation.HashMap as A
import qualified Data.Vector as B


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

getIndexTableAsValueTable :: (Eq value, Hashable value) => Get value -> Get (IndexTable value)
getIndexTableAsValueTable getValue =
  do
    size <- getSize
    associations <- getAssociations size
    return (IndexTable size associations)
  where
    getSize = fromIntegral <$> getInt64le
    getAssociations size = foldM step A.empty (enumFromTo 0 (pred size))
      where
        step hashMap index = do
          value <- getValue
          return (A.insert value index hashMap)

getVector :: Get element -> Get (Vector element)
getVector getElement =
  getSize >>= getElements
  where
    getSize = getInt64le
    getElements size = B.replicateM (fromIntegral size) getElement

getValueTable :: Get value -> Get (ValueTable value)
getValueTable getValue =
  ValueTable <$> getVector getValue
