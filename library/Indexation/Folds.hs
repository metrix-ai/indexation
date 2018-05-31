module Indexation.Folds
where

import Indexation.Prelude
import Indexation.Types
import Control.Foldl
import qualified Data.HashMap.Strict as B
import qualified Data.Serialize as C
import qualified Data.ByteString as D


indexTable :: (Eq value, Hashable value) => Fold value (IndexTable value)
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

serializeToFile :: Serialize value => FilePath -> FoldM IO value (Either IOException ())
serializeToFile filePath =
  lmap C.encode (writeToFile filePath)

writeToFile :: FilePath -> FoldM IO ByteString (Either IOException ())
writeToFile filePath =
  FoldM step init exit
  where
    step :: Either IOException Handle -> ByteString -> IO (Either IOException Handle)
    step fileHandleIfValid bytes =
      case fileHandleIfValid of
        Right fileHandle -> try (D.hPut fileHandle bytes $> fileHandle)
        Left exception -> return (Left exception)
    init :: IO (Either IOException Handle)
    init =
      try (openFile filePath WriteMode)
    exit :: Either IOException Handle -> IO (Either IOException ())
    exit fileHandleIfValid =
      case fileHandleIfValid of
        Right fileHandle -> try (hClose fileHandle)
        Left exception -> return (Left exception)
