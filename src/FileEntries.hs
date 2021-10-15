{-# LANGUAGE NoImplicitPrelude #-}
module FileEntries  ( FileEntries(..)
                    , Entry(..)
                    , FileTreeInfo
                    , mapFileEntries
                    , getFileInfo
                    , fileInfoToEntry
                    , filterFileEntries
                    , mapEntriesList
                    , mapEntries
                    ,setEntries,fileTreeCursor) where

import           Brick.Widgets.FileBrowser (FileInfo (..))
import qualified Brick.Widgets.List        as L
import           Import

filterFileEntries :: (a -> Bool) -> FileEntries a -> FileEntries a
filterFileEntries pred (FileEntries entries) = FileEntries $ concatMap (filterEntry pred) entries

filterEntry :: (a -> Bool) -> Entry a -> [Entry a]
filterEntry pred (FileEntry a) = [FileEntry a | pred a]
filterEntry pred (ExpandedDirectory a entries) =
  case concatMap (filterEntry pred) entries of
    []              -> [ExpandedDirectory a [] | pred a]
    filteredEntries -> [ExpandedDirectory a filteredEntries]

mapFileEntries :: (Entry a -> Entry a) -> FileEntries a -> FileEntries a
mapFileEntries f (FileEntries fs) = FileEntries $ femap f fs
  where
    femap _ [] = []
    femap f (fi@(FileEntry _) : t) = f fi : femap f t
    femap f ((ExpandedDirectory fi dirEntries) : t) =
      let ed = ExpandedDirectory fi (femap f dirEntries)
      in f ed : femap f t

fileInfoToEntry :: FileInfo -> Entry FileTreeInfo
fileInfoToEntry fi = FileEntry ("",fi)

getFileInfo :: Entry FileTreeInfo -> FileInfo
getFileInfo (ExpandedDirectory (_,fi) _) = fi
getFileInfo (FileEntry (_,fi))           = fi

-- | Get the file information for the file under the cursor, if any.
fileTreeCursor :: FileTree n -> Maybe FileTreeInfo
fileTreeCursor ft = snd <$> L.listSelectedElement (fileTreeEntries ft)

mapEntriesList :: (L.GenericList n FileEntries FileTreeInfo -> L.GenericList n FileEntries FileTreeInfo)
           -> FileTree n -> FileTree n
mapEntriesList f ft = ft { fileTreeEntries = f (fileTreeEntries ft)}

mapEntries :: (FileEntries FileTreeInfo -> FileEntries FileTreeInfo)
           -> FileTree n -> FileTree n
mapEntries f ft =
  ft { fileTreeAllEntries = f (fileTreeAllEntries ft)
     , fileTreeEntries = let oldList = fileTreeEntries ft
                         in L.listReplace (f $ L.listElements oldList)
                                          (L.listSelected oldList)
                                          oldList
     }

setEntries :: FileEntries FileTreeInfo -> FileTree n -> FileTree n
setEntries fileEntries ft =
  ft { fileTreeAllEntries = fileEntries
     , fileTreeEntries = let oldList = fileTreeEntries ft
                         in L.listReplace fileEntries
                                          (L.listSelected oldList)
                                          oldList
    }

