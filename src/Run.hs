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

parentOf :: FilePath -> IO FileInfo
parentOf path = FB.getFileInfo ".." $ FP.takeDirectory path

newFileTree :: n -> IO (FileTree n)
newFileTree name = do
  initialCwd <- D.getCurrentDirectory
  entriesResult <- E.try $ FB.entriesForDirectory initialCwd

  let (entries, exc) = case entriesResult of
          Left (e::E.IOException) -> ([], Just e)
          Right es -> (es, Nothing)

  allEntries <- if initialCwd == "/" then return entries else do
      parentResult <- E.try $ parentOf initialCwd
      return $ case parentResult of
          Left (_::E.IOException) -> entries
          Right parent -> parent : entries

  return $ FileTree initialCwd (L.list name (Vec.fromList allEntries) 1) Nothing name

run :: RIO App ()
run = do
  _ <- liftIO $ M.defaultMain theApp =<< newFileTree FileBrowser1
  --FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
  return ()
  
-- | Get the file information for the file under the cursor, if any.
fileBrowserCursor :: FileTree n -> Maybe FileInfo
fileBrowserCursor b = snd <$> L.listSelectedElement (fileTreeEntries b)
  
openFile :: FileTree Name -> IO (FileTree Name)
openFile s =
  case fileBrowserCursor s of
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
drawUI b = [center ui ]
    where
        ui = hCenter $
             renderFileTree b
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

appEvent :: FileTree Name -> BrickEvent Name e -> T.EventM Name (T.Next (FileTree Name))
appEvent b (VtyEvent ev) =
    case ev of
      V.EvKey V.KEsc [] ->
        M.halt b
      V.EvKey V.KEnter [] -> do
        --b' <- FB.handleFileBrowserEvent ev b 
        M.suspendAndResume (openFile b)
      _ -> M.continue b
  
appEvent b _ = M.continue b
--appEvent :: FB.FileBrowser Name -> BrickEvent Name e -> T.EventM Name (T.Next (FB.FileBrowser Name))
--appEvent b (VtyEvent ev) =
--    case ev of
--        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) ->
--            M.halt b
--        V.EvKey V.KEnter [] -> do
--            b' <- FB.handleFileBrowserEvent ev b 
--            M.suspendAndResume (openFile b')
--        _ -> do
--            b' <- FB.handleFileBrowserEvent ev b 
--            M.continue b'
--appEvent b _ = M.continue b

errorAttr :: A.AttrName
errorAttr = "error"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []
--    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow)
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

