module Render (drawUI) where

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

import           Import

import           Brick.Types               (Widget)
import           Brick.Widgets.Center      (center, hCenter)
import           Brick.Widgets.Core        (emptyWidget, hBox, txt, (<=>))
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List        as L
import qualified RIO.Text                  as Text

renderFileTree :: (Ord n, Show n) => FileTree n -> Widget n
renderFileTree fileTree = L.renderList renderFileInfo True $ fileTreeEntries fileTree

renderFileInfo :: Bool -> FileTreeInfo -> Widget n
renderFileInfo _selected (indent,fileInfo) =
  case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
    Right (Just FB.Directory) -> txt $ indent <> Text.pack (FB.fileInfoSanitizedFilename fileInfo)
                                              <> "/"
    _ -> txt $ indent <> Text.pack (FB.fileInfoSanitizedFilename fileInfo)

drawUI :: FileTree Name -> [Widget Name]
drawUI ft = [center ui <=> showError <=> metaInfo ] -- <+> showDebug]
    where
        ui = hCenter $
             renderFileTree ft
        metaInfo = hBox [showMode, txt "   ", showSearch]
        showMode = txt $ tshow $ fileTreeMode ft
        showSearch = txt $ "Search: " <> tshow (fileTreeSearchText ft) <> ""
        showError = case fileTreeErrorMsg ft of
                      Nothing -> emptyWidget
                      Just errMsg -> txt "An error occured while running the external command:"
                                     <=> txt errMsg
        showDebug = txt "Debug info - fileTreeAllEntries:" <=>
                      L.renderList renderFileInfo True (L.list Debug1 (fileTreeAllEntries ft) 1)
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
