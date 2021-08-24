{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FileEntries (FileEntries(..), Entry(..), FileTreeInfo, mapFileEntries, getFileInfo, fileInfoToEntry,
                    filterFileEntries ) where

import           Import

import           Brick.Widgets.FileBrowser (FileInfo (..))
import qualified Brick.Widgets.List        as L

data FileEntries a = FileEntries [Entry a]
  deriving (Show, Eq)

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

data Entry a = ExpandedDirectory a [Entry a]
             | FileEntry a
  deriving (Show, Eq)

-- | FileTreeInfo is an alias for a FileInfo with some text to prepend when rendered,
-- used to indent expanded directory contents.
type FileTreeInfo = (Text, FileInfo)

fileInfoToEntry :: FileInfo -> Entry FileTreeInfo
fileInfoToEntry fi = FileEntry ("",fi)

getFileInfo :: Entry FileTreeInfo -> FileInfo
getFileInfo (ExpandedDirectory (_,fi) _) = fi
getFileInfo (FileEntry (_,fi))           = fi

instance Functor FileEntries where
  fmap f (FileEntries fs) =
    FileEntries $ fmap fmapEntry fs
      where
        fmapEntry (FileEntry fi) = FileEntry (f fi)
        fmapEntry (ExpandedDirectory fi entries) = ExpandedDirectory (f fi) (map fmapEntry entries)

instance Foldable FileEntries where
  foldr f z (FileEntries fs) =
    foldr foldEntry z fs
      where
        foldEntry (FileEntry fi) = f fi
        foldEntry (ExpandedDirectory fi entries) = flip (foldr foldEntry) (FileEntry fi:entries)

instance Traversable FileEntries where
  traverse f (FileEntries fs) =
    FileEntries <$> traverse fEntry fs
      where
        fEntry (FileEntry fi) = FileEntry <$> f fi
        fEntry (ExpandedDirectory fi dirEntries) =
          ExpandedDirectory <$> f fi <*> traverse fEntry dirEntries

instance L.Splittable FileEntries where
  splitAt 0 (FileEntries fs) = (FileEntries [], FileEntries fs)
  splitAt n (FileEntries fs) =
    let (_, ts) = takeEntry n fs
        (_, ds) = dropEntry n fs
    in (FileEntries ts, FileEntries ds)
    where
      takeEntry 0 _ = (0, [])
      takeEntry i [] = (i, [])
      takeEntry i (FileEntry fi : entries) =
        let (i', ls) = takeEntry (i-1) entries
        in (i', FileEntry fi : ls)
      takeEntry i (ExpandedDirectory fi dirEntries : entries) =
        let (i', ls) = takeEntry (i-1) dirEntries
            (i'', ls') = takeEntry i' entries
        in (i'', ExpandedDirectory fi ls : ls')
      dropEntry 0 fs' = (0,fs')
      dropEntry i [] = (i,[])
      dropEntry i (FileEntry _ : entries) = dropEntry (i-1) entries
      dropEntry i (ExpandedDirectory _ dirEntries : entries) =
        let (i', ds') = dropEntry (i-1) (dirEntries ++ entries)
        in (i', ds')

