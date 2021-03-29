{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import hiding (on)

import qualified Control.Exception as E
import qualified Graphics.Vty as V
import qualified Data.Text as Text
import System.Process.Typed (proc, runProcess)
import RIO.List (headMaybe)

import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import Brick.Types (Widget, BrickEvent(..))
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core 
  ((<=>), txt, withDefAttr, emptyWidget, vBox, padTop)
import Brick.Util (on, fg)

data Name = FileBrowser1
  deriving (Eq, Show, Ord)

run :: RIO App ()
run = do
  b <- liftIO $ M.defaultMain theApp =<< FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
  logInfo $ "Selected entry: " <> displayShow (FB.fileBrowserSelection b)
  
  --createProcess $ shell $ "nvr " ++ (show (FB.fileBrowserSelection b))
  
openFile :: FB.FileBrowser Name -> IO (FB.FileBrowser Name)
openFile s =
  case FB.fileBrowserCursor s of
    Nothing -> return s -- $ ExitFailure 1
    Just fileInfo -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
        Right (Just FB.RegularFile) -> runProcess (proc "nvr" [FB.fileInfoFilePath fileInfo]) 
                                       >> (return s)
        _ -> return s

drawUI :: FB.FileBrowser Name -> [Widget Name]
drawUI b = [center $ ui <=> help]
    where
        ui = hCenter $
             FB.renderFileBrowser True b
        help = padTop (T.Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ E.displayException e
                    , hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

appEvent :: FB.FileBrowser Name -> BrickEvent Name e -> T.EventM Name (T.Next (FB.FileBrowser Name))
appEvent b (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) ->
            M.halt b
        V.EvKey V.KEnter [] -> do
            b' <- FB.handleFileBrowserEvent ev b 
            M.suspendAndResume (openFile b')
        _ -> do
            b' <- FB.handleFileBrowserEvent ev b 
            M.continue b'
appEvent b _ = M.continue b

errorAttr :: A.AttrName
errorAttr = "error"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow)
    , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
    , (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue)
    , (FB.fileBrowserDirectoryAttr, fg V.blue)
    , (FB.fileBrowserBlockDeviceAttr, fg V.magenta)
    , (FB.fileBrowserCharacterDeviceAttr, fg V.green)
    , (FB.fileBrowserNamedPipeAttr, fg V.yellow)
    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (FB.fileBrowserUnixSocketAttr, fg V.red)
    , (FB.fileBrowserSelectedAttr, V.white `on` V.magenta)
    , (errorAttr, fg V.red)
    ]

theApp :: M.App (FB.FileBrowser Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

