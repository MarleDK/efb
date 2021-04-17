{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (run) where

import Import hiding (on)
import FileEntries (FileEntries(..), FileTreeInfo, Entry(..), mapFileEntries, getFileInfo, fileInfoToEntry, filterFileEntries)

import qualified Control.Exception as E
import qualified Graphics.Vty as V
import qualified Data.Text as Text
import System.Process.Typed (proc, runProcess_)
import qualified System.Directory as D
import qualified System.FilePath as FP
import           RIO.Char (isDigit)
import           RIO.Char.Partial (digitToInt)

import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.FileBrowser (FileInfo(..), FileInfo(..))

import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import Brick.Types (Widget, BrickEvent(..))
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core 
  ((<+>), (<=>), txt, withDefAttr, emptyWidget, hBox, vBox, padTop, hLimit)
import Brick.Util (on, fg)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Name = FileBrowser1
          | Debug1
  deriving (Eq, Show, Ord)

data Mode = Normal
          | Search
  deriving (Eq, Show)

data Config n = 
  Config { configName :: n
         , configCommand :: (String,[String])
         }
  deriving (Eq, Show)

fileTreeName :: FileTree n -> n
fileTreeName = configName . fileTreeConfig

fileTreeCommand :: FileTree n -> (String,[String])
fileTreeCommand = configCommand . fileTreeConfig

data FileTree n = 
  FileTree { fileTreeWorkingDirectory :: FilePath
           , fileTreeEntries :: L.GenericList n FileEntries FileTreeInfo
           , fileTreeAllEntries :: FileEntries FileTreeInfo
           , fileTreeMode :: Mode
           , fileTreeSearchText :: Text
           , fileTreeQuantifier :: Int
           , fileTreeConfig :: Config n
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
setMode mode ft = updateSearch $ ft { fileTreeMode = mode}


--------------------------------------------------------------------------------
-- Run
--------------------------------------------------------------------------------

run :: RIO App ()
run = do
  command <- getCommand
  let config = Config { configName = FileBrowser1
                      , configCommand = command
                      }
  _ <- liftIO $ M.defaultMain theApp =<< newFileTree config
  return ()
  
data InputParseException = InputParseException String
  deriving Show
instance Exception InputParseException

getCommand :: RIO App (String, [String])
getCommand = do
  options <- ask
  let command = optionsCommand $ appOptions options
  case words command of
    [] -> throwIO (InputParseException "Somehow optsapplicative could parse your COMMAND, but it does not seem to contain any words. This can happen if you try to use spaces as your command, or use ''.")
    h:t -> return (h,t)

parentOf :: FilePath -> IO FileInfo
parentOf path = FB.getFileInfo ".." $ FP.takeDirectory path

newFileTree :: Config n -> IO (FileTree n)
newFileTree config = do
  initialCwd <- D.getCurrentDirectory
  updateWorkingDirectory config initialCwd

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
drawUI ft = [center ui <=> metaInfo ] -- <+> showDebug]
    where
        ui = hCenter $
             renderFileTree ft
        metaInfo = hBox [showMode, txt "   ", showSearch]
        showMode = txt $ tshow $ fileTreeMode ft
        showSearch = txt $ "Search: " <> tshow (fileTreeSearchText ft) <> ""
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

--------------------------------------------------------------------------------
-- Handling events
--------------------------------------------------------------------------------

-- | get contents of a directory to show in the file tree
getDirectoryContent :: FilePath -> IO ([FileInfo])
getDirectoryContent workingDirectory = do
  entriesResult <- E.try $ FB.entriesForDirectory workingDirectory

  let (entries, _) = case entriesResult of
          Left (e::E.IOException) -> ([], Just e)
          Right es -> (es, Nothing)

  return entries

-- | update the file tree to a new working directory
updateWorkingDirectory :: Config n -> FilePath -> IO (FileTree n)
updateWorkingDirectory conf workingDirectory = do
  entries <- getDirectoryContent workingDirectory
  allEntries <- if workingDirectory == "/" then return entries else do
      parentResult <- E.try $ parentOf workingDirectory
      return $ case parentResult of
          Left (_::E.IOException) -> entries
          Right parent -> parent : entries
  let fileEntries = FileEntries (map fileInfoToEntry allEntries)
  return $ FileTree { fileTreeWorkingDirectory = workingDirectory 
                    , fileTreeEntries = (L.list (configName conf) fileEntries 1) 
                    , fileTreeAllEntries = fileEntries 
                    , fileTreeMode = Normal
                    , fileTreeSearchText = ""
                    , fileTreeQuantifier = 1
                    , fileTreeConfig = conf
                    }

-- | run the command on the file
runFile :: (String,[String]) -> FileInfo -> IO ()
runFile (command,options) fileInfo = 
  case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
    Right (Just FB.RegularFile) -> 
      runProcess_ (proc command (options ++ [FB.fileInfoFilePath fileInfo]))
    _ -> return ()

-- | Handle "entering" a file. Looks at the type of the file, if it is a directory
-- it changes into that directory, if it is a regular file, it runs the given command.
enterFile :: FileTree Name -> T.EventM Name (T.Next (FileTree Name))
enterFile ft = 
  case fileTreeCursor ft of
    Nothing -> M.continue ft
    Just (_,entry) -> 
      case FB.fileInfoFileType entry of 
        Just FB.Directory     ->
          M.suspendAndResume $ updateWorkingDirectory (fileTreeConfig ft) 
                                                      (FB.fileInfoFilePath entry)
        Just FB.RegularFile   -> 
          M.suspendAndResume (runFile (fileTreeCommand ft) entry >> return ft)

        Just FB.SymbolicLink  -> case fileInfoLinkTargetType entry of
            Just FB.Directory   -> 
              M.suspendAndResume $ updateWorkingDirectory (fileTreeConfig ft) 
                                                          (FB.fileInfoFilePath entry)
            Just FB.RegularFile -> 
              M.suspendAndResume (runFile (fileTreeCommand ft) entry >> return ft)
            _ -> M.continue ft

        _ -> M.continue ft

addIndentation :: Text -> FileInfo -> FileTreeInfo
addIndentation indent fi = (indent <> "  ", fi)

-- | flipExpansion expands a directory, if it is collapsed, and collapses a directory if it is expanded
flipExpansion :: [Entry FileTreeInfo] -- ^ The contents of the directory to maybe expand
              -> FileInfo             -- ^ The fileinfo of the file to find
              -> Entry FileTreeInfo   -- ^ The fileInfo of the list
              -> Entry FileTreeInfo
flipExpansion newDirectoryContents fileInfo entry = 
  if getFileInfo entry == fileInfo 
  then case entry of
        ExpandedDirectory fi _ -> FileEntry fi
        FileEntry fi -> ExpandedDirectory fi newDirectoryContents
  else entry

-- | expandDirectory looks at the current selection, if it is a directory, it flips the expansion
-- of it with flipExpansion
expandDirectory :: FileTree Name -> T.EventM Name (T.Next (FileTree Name))
expandDirectory ft =
  case fileTreeCursor ft of
    Nothing                 -> M.continue ft
    Just (indent,fileInfo)  -> 
      case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of -- See if the file is a directory
        Right (Just FB.Directory) -> do
          newDirectoryContents <- map (FileEntry . addIndentation indent) <$> -- Make it an Entry type
            liftIO (getDirectoryContent (fileInfoFilePath fileInfo)) -- get contents of directory

          newFileEntries <- return $ mapFileEntries (flipExpansion newDirectoryContents fileInfo) 
                                   $ fileTreeAllEntries ft

          M.continue $ mapEntries (const newFileEntries) ft 

        _                         -> M.continue ft

-- | Update the FileTree to the current search text
updateSearch :: FileTree n -> FileTree n
updateSearch ft = 
  let oldList = fileTreeEntries ft
      searchText = fileTreeSearchText ft
      searchPred (_,fi) = Text.toLower searchText `Text.isInfixOf` 
                          (Text.toLower $ Text.pack $ fileInfoSanitizedFilename fi)
      elementsBeforeSelected = fst $ L.splitAt (fromMaybe 0 (L.listSelected oldList)) 
                                               (L.listElements oldList)
      newIndex = length $ filterFileEntries searchPred elementsBeforeSelected 
  in ft {fileTreeEntries = L.listReplace (filterFileEntries searchPred (fileTreeAllEntries ft)) 
                                         (Just newIndex) oldList
        }

-- | map over the search text, and update the FileTree to correspond to the new search
updateSearchText :: (Text.Text -> Text.Text) -> FileTree n -> FileTree n
updateSearchText f ft = 
  let newFt = ft { fileTreeSearchText = f (fileTreeSearchText ft)}
  in updateSearch newFt

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
    V.EvKey V.KEsc []         -> M.continue $ setMode (Normal) ft
    V.EvKey V.KEnter []       -> M.continue $ setMode (Normal) ft
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
  -- Search
    V.EvKey (V.KChar 's') []  -> M.continue $ setMode Search ft
    V.EvKey (V.KChar 'c') []  -> M.continue $ updateSearchText (const "") ft 
  -- Quit
    V.EvKey (V.KChar 'q') []  -> M.halt ft
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
