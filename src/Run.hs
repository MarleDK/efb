{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (run) where

import Import hiding (on)
import FileEntries (FileEntries(..), FileTreeInfo, Entry(..), mapFileEntries, getFileInfo, fileInfoToEntry)

import qualified Control.Exception as E
import qualified Graphics.Vty as V
import qualified Data.Text as Text
import System.Process.Typed (proc, runProcess)
import qualified System.Directory as D
import qualified System.FilePath as FP

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

data Mode = Normal
          | Search Text.Text
  deriving (Eq, Show)

mapSearchMode :: (Text.Text -> Text.Text) -> Mode -> Mode
mapSearchMode f (Search t) = Search (f t)
mapSearchMode _ mode = mode

data FileTree n = 
  FileTree { fileTreeWorkingDirectory :: FilePath
           , fileTreeEntries :: L.GenericList n FileEntries FileTreeInfo
           , fileTreeAllEntries :: FileEntries FileTreeInfo
           , fileTreeMode :: Mode
           , fileTreeName :: n
           } 
  deriving (Show)

mapEntriesList :: (L.GenericList n FileEntries FileTreeInfo -> L.GenericList n FileEntries FileTreeInfo) 
           -> FileTree n -> FileTree n
mapEntriesList f ft = ft { fileTreeEntries = f (fileTreeEntries ft)}

mapEntries :: (FileEntries FileTreeInfo -> FileEntries FileTreeInfo) 
           -> FileTree n -> FileTree n
mapEntries f ft = ft { fileTreeAllEntries = f (fileTreeAllEntries ft)
                     , fileTreeEntries = let oldList = fileTreeEntries ft
                                         in L.listReplace (f $ L.listElements oldList) (L.listSelected oldList) oldList}

setMode :: Mode -> FileTree n -> FileTree n
setMode mode ft = ft { fileTreeMode = mode}


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
fileTreeCursor :: FileTree n -> Maybe FileTreeInfo
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

renderFileInfo :: Bool -> FileTreeInfo -> Widget n
renderFileInfo _selected (indent,fileInfo) = 
  case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
    Right (Just FB.Directory) -> txt $ indent <> Text.pack (FB.fileInfoSanitizedFilename fileInfo) <> "/"
    _ -> txt $ indent <> Text.pack (FB.fileInfoSanitizedFilename fileInfo)

drawUI :: FileTree Name -> [Widget Name]
drawUI ft = [center ui <=> showMode]
    where
        ui = hCenter $
             renderFileTree ft
        showMode = vBox [txt $ tshow $ fileTreeMode ft]
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
  let fileEntries = FileEntries (map fileInfoToEntry allEntries)
  return $ FileTree workingDirectory (L.list name fileEntries 1) fileEntries Normal name

openFile :: FileTree Name -> IO (FileTree Name)
openFile s =
  case fileTreeCursor s of
    Nothing -> return s -- $ ExitFailure 1
    Just (_,fileInfo) -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
        Right (Just FB.RegularFile) -> runProcess (proc "nvr" [FB.fileInfoFilePath fileInfo]) 
                                       >> (return s)
        _ -> return s

enterFile :: FileTree Name -> T.EventM Name (T.Next (FileTree Name))
enterFile ft = 
  case fileTreeCursor ft of
    Nothing -> M.continue ft
    Just (_,entry) -> 
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

addIndentation :: Text -> FileInfo -> FileTreeInfo
addIndentation indent fi = (indent <> "  ", fi)

-- | flipExpansion expands a directory, if it is collapsed, and collapses a directory if it is expanded
flipExpansion :: [Entry FileTreeInfo] -- ^ The contents of the directory to maybe expand
              -> FileInfo             -- ^ The fileinfo of the file to find
              -> Entry FileTreeInfo   -- ^ The fileInfo of the list
              -> Entry FileTreeInfo
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
    Just (indent,fileInfo) -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of -- See if the file is a directory
        Right (Just FB.Directory) -> do
          newDirectoryContents <- 
            map (FileEntry . addIndentation indent) <$> -- Make it an Entry type
              liftIO (getDirectoryContent (fileInfoFilePath fileInfo)) -- get contents of directory

          newFileEntries <- return $ mapFileEntries (flipExpansion newDirectoryContents fileInfo) 
                                   $ L.listElements $ fileTreeEntries ft

          M.continue $ mapEntries (const newFileEntries) ft --(\oldList -> L.listReplace newFileEntries (L.listSelected oldList) oldList) ft

        _ -> M.continue ft

updateSearchText :: (Text.Text -> Text.Text) -> FileTree n -> FileTree n
updateSearchText f ft = 
  let newFt = ft { fileTreeMode = mapSearchMode f (fileTreeMode ft)}
  in newFt

appEvent :: FileTree Name -> BrickEvent Name e -> T.EventM Name (T.Next (FileTree Name))
appEvent ft (VtyEvent ev) =
    case ev of
      V.EvKey V.KEnter [] -> do
        enterFile ft
      _ -> 
        case fileTreeMode ft of 
          Normal -> appEventNormal ft ev
          Search _ -> appEventSearch ft ev
        
appEvent ft _ = M.continue ft

-- | The event handling function used in search mode
appEventSearch :: FileTree Name -> V.Event -> T.EventM Name (T.Next (FileTree Name))
appEventSearch ft ev =
    case ev of
      V.EvKey (V.KChar char) [] ->
        M.continue $ updateSearchText (flip Text.snoc char) ft 
      V.EvKey V.KEsc [] ->
        M.continue $ setMode (Normal) ft
      _ -> M.continue ft

-- | The event handling function used in normal mode
appEventNormal :: FileTree Name -> V.Event -> T.EventM Name (T.Next (FileTree Name))
appEventNormal ft ev =
    case ev of
      V.EvKey (V.KChar 'q') [] ->
        M.halt ft
      V.EvKey (V.KChar 'e') [] ->
        expandDirectory ft
      V.EvKey (V.KChar 'j') [] ->
        M.continue $ mapEntriesList (L.listMoveBy 1) ft
      V.EvKey V.KDown [] ->
        M.continue $ mapEntriesList (L.listMoveBy 1) ft
      V.EvKey V.KUp [] ->
        M.continue $ mapEntriesList (L.listMoveBy (-1)) ft
      V.EvKey (V.KChar 'k') [] ->
        M.continue $ mapEntriesList (L.listMoveBy (-1)) ft
      V.EvKey (V.KChar 's') [] ->
        M.continue $ setMode (Search "") ft
      _ -> M.continue ft
  

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
