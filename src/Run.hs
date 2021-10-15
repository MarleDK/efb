{-# LANGUAGE NoImplicitPrelude #-}
module Run (run) where

import           FileEntries        (mapEntriesList)
import           Import             hiding (on)
import           Render             (drawUI)

import qualified Graphics.Vty       as V
import           RIO.Char           (isDigit)
import           RIO.Char.Partial   (digitToInt)
import qualified RIO.Text           as Text
import qualified System.Directory   as D

import qualified Brick.Main         as M
import qualified Brick.Widgets.List as L

import qualified Brick.AttrMap      as A
import           Brick.Types        (BrickEvent (..))
import qualified Brick.Types        as T
import           Brick.Util         (on)
import           UpdateFunctions    (enterFile, expandDirectory,
                                     goToParentDirectory, refreshFileTree,
                                     setMode, updateSearchText,
                                     updateWorkingDirectory)

--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

run :: RIO App ()
run = do
  command <- optionsCommand <$> appOptions <$> ask
  let config = Config { configName = FileBrowser1
                      , configCommand = command
                      }
  _ <- liftIO $ M.defaultMain theApp =<< newFileTree config
  return ()

newtype InputParseException = InputParseException String
  deriving Show
instance Exception InputParseException

newFileTree :: Config n -> IO (FileTree n)
newFileTree config = do
  initialCwd <- D.getCurrentDirectory
  updateWorkingDirectory config initialCwd

theApp :: M.App (FileTree Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

--------------------------------------------------------------------------------
-- Handling events
--------------------------------------------------------------------------------

-- | The event handler
appEvent :: FileTree Name -> BrickEvent Name e -> T.EventM Name (T.Next (FileTree Name))
appEvent ft (VtyEvent ev) =
  case fileTreeMode ft of
    Normal -> appEventNormal ft ev
    Search -> appEventSearch ft ev
appEvent ft _ = M.continue ft

-- | The event handling function used in search mode
appEventSearch :: FileTree Name -> V.Event -> T.EventM Name (T.Next (FileTree Name))
appEventSearch ft ev =
  case ev of
  -- Writing
    -- Write given character
    V.EvKey (V.KChar char) [] -> M.continue $ updateSearchText (flip Text.snoc char) ft
    -- delete last char
    V.EvKey V.KBS []          -> M.continue $ updateSearchText (Text.dropEnd 1) ft
  -- Movement
    V.EvKey V.KDown []        -> M.continue $ mapEntriesList (L.listMoveBy 1) ft
    V.EvKey V.KUp []          -> M.continue $ mapEntriesList (L.listMoveBy (-1)) ft
  -- Return to normal mode
    V.EvKey V.KEsc []         -> M.continue $ setMode Normal ft
    V.EvKey V.KEnter []       -> M.continue $ setMode Normal ft
    _ -> M.continue ft

-- | The event handling function used in normal mode
appEventNormal :: FileTree Name -> V.Event -> T.EventM Name (T.Next (FileTree Name))
appEventNormal oldFt ev =
  let n = fileTreeQuantifier oldFt -- get the current quantifier
      ft = oldFt {fileTreeQuantifier = 1} -- reset the quantifier for updated ft
  in case ev of
  -- Entering directories or run command on file
    V.EvKey V.KEnter []       -> enterFile ft
    V.EvKey (V.KChar 'l') []  -> enterFile ft
    V.EvKey (V.KChar 'e') []  -> expandDirectory ft
  -- Movement
    V.EvKey (V.KChar 'j') []  -> M.continue $ mapEntriesList (L.listMoveBy n) ft
    V.EvKey V.KDown []        -> M.continue $ mapEntriesList (L.listMoveBy n) ft
    V.EvKey (V.KChar 'k') []  -> M.continue $ mapEntriesList (L.listMoveBy (-n)) ft
    V.EvKey V.KUp []          -> M.continue $ mapEntriesList (L.listMoveBy (-n)) ft
    V.EvKey (V.KChar x) []  | isDigit x ->
      M.continue $ ft {fileTreeQuantifier = digitToInt x}
    V.EvKey (V.KChar 'g') []  -> M.continue $ mapEntriesList (L.listMoveTo 0) ft
    V.EvKey (V.KChar 'G') []  -> M.continue $ mapEntriesList (L.listMoveTo (-1)) ft
    V.EvKey (V.KChar 'h') []  -> M.suspendAndResume $ goToParentDirectory ft
  -- Search
    V.EvKey (V.KChar 's') []  -> M.continue $ setMode Search ft
    V.EvKey (V.KChar 'c') []  -> M.continue $ updateSearchText (const "") ft
  -- Management
    V.EvKey (V.KChar 'q') []  -> M.halt ft
    V.EvKey (V.KChar 'r') []  -> M.suspendAndResume $ refreshFileTree ft
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
