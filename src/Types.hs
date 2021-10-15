{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           Brick.Widgets.FileBrowser (FileInfo (..))
import qualified Brick.Widgets.List        as L
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsCommand:: !String
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  -- Add other app-specific configuration information here
  }

data Name = FileBrowser1
          | Debug1
  deriving (Eq, Show, Ord)

data Mode = Normal
          | Search
  deriving (Eq, Show)

data Config n =
  Config { configName    :: n
         , configCommand :: String
         }
  deriving (Eq, Show)

fileTreeName :: FileTree n -> n
fileTreeName = configName . fileTreeConfig

fileTreeCommand :: FileTree n -> String
fileTreeCommand = configCommand . fileTreeConfig

data FileTree n =
  FileTree { fileTreeWorkingDirectory :: FilePath
           , fileTreeEntries :: L.GenericList n FileEntries FileTreeInfo
           , fileTreeAllEntries :: FileEntries FileTreeInfo
           , fileTreeMode :: Mode
           , fileTreeSearchText :: Text
           , fileTreeQuantifier :: Int
           , fileTreeErrorMsg :: Maybe Text
           , fileTreeConfig :: Config n
           }
  deriving (Show)


instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

-------------------------------------------------------------
-- | FileEntries Instances
-------------------------------------------------------------
newtype FileEntries a = FileEntries [Entry a]
  deriving (Show, Eq)

data Entry a = ExpandedDirectory a [Entry a]
             | FileEntry a
  deriving (Show, Eq)

-- | FileTreeInfo is an alias for a FileInfo with some text to prepend when rendered,
-- used to indent expanded directory contents.
type FileTreeInfo = (Text, FileInfo)

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
