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

data Name = FileBrowser1
  deriving (Eq, Show, Ord)

data FileTree n = 
  FileTree { fileTreeWorkingDirectory :: FilePath
           , fileTreeEntries :: L.List n FileInfo
           , fileTreeSearchString :: Maybe Text.Text
           , fileTreeName :: n
           } 

--setEntries :: FileTree n -> L.List n FileInfo -> FileTree n
--setEntries (FileTree wd _ ss name) newEntries = (FileTree wd newEntries ss name)

mapEntries :: (L.List n FileInfo -> L.List n FileInfo) -> FileTree n -> FileTree n
mapEntries f (FileTree wd entries ss name) = (FileTree wd (f entries) ss name)

--setSearchString :: FileTree n -> Maybe Text.Text -> FileTree n
--setSearchString (FileTree wd entries _ name) newSearchString = (FileTree wd entries newSearchString name)

parentOf :: FilePath -> IO FileInfo
parentOf path = FB.getFileInfo ".." $ FP.takeDirectory path

newFileTree :: n -> IO (FileTree n)
newFileTree name = do
  initialCwd <- D.getCurrentDirectory
  updateWorkingDirectory name initialCwd

updateWorkingDirectory :: n -> FilePath -> IO (FileTree n)
updateWorkingDirectory name workingDirectory = do
  entriesResult <- E.try $ FB.entriesForDirectory workingDirectory

  let (entries, _) = case entriesResult of
          Left (e::E.IOException) -> ([], Just e)
          Right es -> (es, Nothing)

  allEntries <- if workingDirectory == "/" then return entries else do
      parentResult <- E.try $ parentOf workingDirectory
      return $ case parentResult of
          Left (_::E.IOException) -> entries
          Right parent -> parent : entries

  return $ FileTree workingDirectory (L.list name (Vec.fromList allEntries) 1) Nothing name

run :: RIO App ()
run = do
  _ <- liftIO $ M.defaultMain theApp =<< newFileTree FileBrowser1
  return ()
  
-- | Get the file information for the file under the cursor, if any.
fileTreeCursor :: FileTree n -> Maybe FileInfo
fileTreeCursor ft = snd <$> L.listSelectedElement (fileTreeEntries ft)
  
openFile :: FileTree Name -> IO (FileTree Name)
openFile s =
  case fileTreeCursor s of
    Nothing -> return s -- $ ExitFailure 1
    Just fileInfo -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
        Right (Just FB.RegularFile) -> runProcess (proc "nvr" [FB.fileInfoFilePath fileInfo]) 
                                       >> (return s)
        _ -> return s

renderFileTree :: (Ord n, Show n) => FileTree n -> Widget n
renderFileTree fileTree = L.renderList renderFileInfo True $ fileTreeEntries fileTree

renderFileInfo :: Bool -> FileInfo -> Widget n
renderFileInfo _selected fileInfo = txt $ Text.pack $ FB.fileInfoSanitizedFilename fileInfo

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


appEvent :: FileTree Name -> BrickEvent Name e -> T.EventM Name (T.Next (FileTree Name))
appEvent ft (VtyEvent ev) =
    case ev of
      V.EvKey V.KEsc [] ->
        M.halt ft
      V.EvKey V.KEnter [] -> do
        enterFile ft
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

theApp :: M.App (FileTree Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

