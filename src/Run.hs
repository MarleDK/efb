{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (run) where

import Import hiding (on)

import qualified Control.Exception as E
import qualified Graphics.Vty as V
import qualified Data.Text as Text
import System.Process.Typed (proc, runProcess)
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified RIO.Vector.Boxed as Vec

import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.FileBrowser (FileInfo(..), FileInfo(..))

import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import Brick.Types (Widget, BrickEvent(..))
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core 
  ((<=>), txt, withDefAttr, emptyWidget, vBox, padTop)
import Brick.Util (on, fg)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Name = FileBrowser1
  deriving (Eq, Show, Ord)

data FileTree n = 
  FileTree { fileTreeWorkingDirectory :: FilePath
           , fileTreeEntries :: L.GenericList n FileEntries FileInfo
           , fileTreeSearchString :: Maybe Text.Text
           , fileTreeName :: n
           } 
  deriving (Show)

data FileEntries a = FileEntries [Entry a]
  deriving (Show, Eq)

mapFileEntries :: (Entry a -> Entry a) -> FileEntries a -> FileEntries a
mapFileEntries f (FileEntries fs) = FileEntries $ map f fs


data Entry a = ExpandedDirectory a [Entry a]
             | FileEntry a
  deriving (Show, Eq)

getFileInfo :: Entry a -> a
getFileInfo (ExpandedDirectory a _) = a
getFileInfo (FileEntry a) = a

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
        foldEntry (FileEntry fi) = (f fi)
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
            (i'', ls') = takeEntry (i') entries
        in (i'', ExpandedDirectory fi ls : ls')
      dropEntry 0 fs' = (0,fs')
      dropEntry i [] = (i,[])
      dropEntry i (FileEntry _ : entries) = dropEntry (i-1) entries
      dropEntry i (ExpandedDirectory _ dirEntries : entries) = 
        let (i', _) = dropEntry (i-1) dirEntries
            (i'', ds') = dropEntry i' entries
        in (i'', ds')


--setEntries :: FileTree n -> L.List n FileInfo -> FileTree n
--setEntries (FileTree wd _ ss name) newEntries = (FileTree wd newEntries ss name)

mapEntries :: (L.GenericList n FileEntries FileInfo -> L.GenericList n FileEntries FileInfo) 
           -> FileTree n -> FileTree n
mapEntries f (FileTree wd entries ss name) = (FileTree wd (f entries) ss name)

--setSearchString :: FileTree n -> Maybe Text.Text -> FileTree n
--setSearchString (FileTree wd entries _ name) newSearchString = (FileTree wd entries newSearchString name)

--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

run :: RIO App ()
run = do
  _ <- liftIO $ M.defaultMain theApp =<< newFileTree FileBrowser1
  return ()
  
parentOf :: FilePath -> IO FileInfo
parentOf path = FB.getFileInfo ".." $ FP.takeDirectory path

newFileTree :: n -> IO (FileTree n)
newFileTree name = do
  initialCwd <- D.getCurrentDirectory
  updateWorkingDirectory name initialCwd

-- | Get the file information for the file under the cursor, if any.
fileTreeCursor :: FileTree n -> Maybe FileInfo
fileTreeCursor ft = snd <$> L.listSelectedElement (fileTreeEntries ft)

theApp :: M.App (FileTree Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
  
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderFileTree :: (Ord n, Show n) => FileTree n -> Widget n
renderFileTree fileTree = L.renderList renderFileInfo True $ fileTreeEntries fileTree

renderFileInfo :: Bool -> FileInfo -> Widget n
renderFileInfo _selected fileInfo = 
  case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
    Right (Just FB.Directory) -> txt $ Text.pack (FB.fileInfoSanitizedFilename fileInfo) <> "/"
    _ -> txt $ Text.pack $ FB.fileInfoSanitizedFilename fileInfo

drawUI :: FileTree Name -> [Widget Name]
drawUI ft = [center ui ]
    where
        ui = hCenter $
             renderFileTree ft
--        help = padTop (T.Pad 1) $
--               vBox [ case FB.fileBrowserException b of
--                          Nothing -> emptyWidget
--                          Just e -> hCenter $ withDefAttr errorAttr $
--                                    txt $ Text.pack $ E.displayException e
--                    , hCenter $ txt "Up/Down: select"
--                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
--                    , hCenter $ txt "Enter: change directory or select file"
--                    , hCenter $ txt "Esc: quit"
--                    ]

--------------------------------------------------------------------------------
-- Handling events
--------------------------------------------------------------------------------

getDirectoryContent :: FilePath -> IO ([FileInfo])
getDirectoryContent workingDirectory = do
  entriesResult <- E.try $ FB.entriesForDirectory workingDirectory

  let (entries, _) = case entriesResult of
          Left (e::E.IOException) -> ([], Just e)
          Right es -> (es, Nothing)

  return entries

updateWorkingDirectory :: n -> FilePath -> IO (FileTree n)
updateWorkingDirectory name workingDirectory = do
  entries <- getDirectoryContent workingDirectory
  allEntries <- if workingDirectory == "/" then return entries else do
      parentResult <- E.try $ parentOf workingDirectory
      return $ case parentResult of
          Left (_::E.IOException) -> entries
          Right parent -> parent : entries
  return $ FileTree workingDirectory (L.list name (FileEntries (map FileEntry allEntries)) 1) Nothing name

openFile :: FileTree Name -> IO (FileTree Name)
openFile s =
  case fileTreeCursor s of
    Nothing -> return s -- $ ExitFailure 1
    Just fileInfo -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
        Right (Just FB.RegularFile) -> runProcess (proc "nvr" [FB.fileInfoFilePath fileInfo]) 
                                       >> (return s)
        _ -> return s

enterFile :: FileTree Name -> T.EventM Name (T.Next (FileTree Name))
enterFile ft = 
  case fileTreeCursor ft of
    Nothing -> M.continue ft
    Just entry -> 
      case FB.fileInfoFileType entry of 
        Just FB.Directory ->
          M.suspendAndResume $ updateWorkingDirectory (fileTreeName ft) (FB.fileInfoFilePath entry)

        Just FB.SymbolicLink -> 
          case fileInfoLinkTargetType entry of
            Just FB.Directory -> 
              M.suspendAndResume $ updateWorkingDirectory (fileTreeName ft) (FB.fileInfoFilePath entry)
            _ -> M.continue ft

        Just FB.RegularFile -> M.suspendAndResume (openFile ft)
        _ -> M.continue ft

-- | flipExpansion expands a directory, if it is collapsed, and collapses a directory if it is expanded
flipExpansion :: [Entry FileInfo] -- ^ The contents of the directory to maybe expand
              -> FileInfo         -- ^ The fileinfo of the file to find
              -> Entry FileInfo   -- ^ The fileInfo of the list
              -> Entry FileInfo
flipExpansion newDirectoryContents fileInfo entry = if getFileInfo entry == fileInfo 
                                 then case entry of
                                        ExpandedDirectory fi _ -> FileEntry fi
                                        FileEntry fi -> ExpandedDirectory fi newDirectoryContents
                                 else entry
-- | expandDirectory looks at the current selection, if it is a directory, it flips the expansion
-- of it with flipExpansion
expandDirectory :: FileTree Name -> T.EventM Name (T.Next (FileTree Name))
expandDirectory ft =
  case fileTreeCursor ft of
    Nothing -> M.continue ft
    Just fileInfo -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of -- See if the file is a directory
        Right (Just FB.Directory) -> do
          newDirectoryContents <- 
            map FileEntry <$> -- Make it an Entry type
              liftIO (getDirectoryContent (fileInfoFilePath fileInfo)) -- get contents of directory

          newFileEntries <- return $ mapFileEntries (flipExpansion newDirectoryContents fileInfo) 
                                   $ L.listElements $ fileTreeEntries ft

          M.continue $ mapEntries (\oldList -> L.listReplace newFileEntries (L.listSelected oldList) oldList) ft

        _ -> M.continue ft

appEvent :: FileTree Name -> BrickEvent Name e -> T.EventM Name (T.Next (FileTree Name))
appEvent ft (VtyEvent ev) =
    case ev of
      V.EvKey V.KEsc [] ->
        M.halt ft
      V.EvKey V.KEnter [] -> do
        enterFile ft
      V.EvKey (V.KChar 'e') [] ->
        expandDirectory ft
      V.EvKey (V.KChar 'j') [] ->
        M.continue $ mapEntries (L.listMoveBy 1) ft
      V.EvKey V.KDown [] ->
        M.continue $ mapEntries (L.listMoveBy 1) ft
      V.EvKey V.KUp [] ->
        M.continue $ mapEntries (L.listMoveBy (-1)) ft
      V.EvKey (V.KChar 'k') [] ->
        M.continue $ mapEntries (L.listMoveBy (-1)) ft
      _ -> M.continue ft
  
appEvent ft _ = M.continue ft

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

errorAttr :: A.AttrName
errorAttr = "error"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr 
    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow)
    ]
--    , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
--    , (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue)
--    , (FB.fileBrowserDirectoryAttr, fg V.blue)
--    , (FB.fileBrowserBlockDeviceAttr, fg V.magenta)
--    , (FB.fileBrowserCharacterDeviceAttr, fg V.green)
--    , (FB.fileBrowserNamedPipeAttr, fg V.yellow)
--    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
--    , (FB.fileBrowserUnixSocketAttr, fg V.red)
--    , (FB.fileBrowserSelectedAttr, V.white `on` V.magenta)
--    , (errorAttr, fg V.red)
--    ]
