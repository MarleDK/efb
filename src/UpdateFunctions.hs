{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpdateFunctions ( updateWorkingDirectory
                       , updateSearchText
                       , setMode
                       , enterFile
                       , expandDirectory
                       , goToParentDirectory
                       , refreshFileTree) where

import qualified Data.ByteString.Lazy      as BS
import           Import
import           System.Process.Typed      (readProcess, shell)

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import           Brick.Widgets.FileBrowser (FileInfo (..))
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List        as L
import qualified Control.Exception         as E
import           FileEntries               (fileInfoToEntry, fileTreeCursor,
                                            filterFileEntries, getFileInfo,
                                            mapEntries, mapFileEntries,
                                            setEntries)
import           RIO.FilePath              ((</>))
import qualified RIO.Text                  as Text

-- | UpdateFunctions are a collection of functions to update the state of the file browser.
--   A lot of them requires IO, to look at the file system, and other things.

setMode :: Mode -> FileTree n -> FileTree n
setMode mode ft = updateSearch $ ft { fileTreeMode = mode}

setErrorMsg :: FileTree n -> Maybe (ExitCode, BS.ByteString, BS.ByteString) -> FileTree n
setErrorMsg ft Nothing = ft
setErrorMsg ft (Just (ExitSuccess,_,_)) = ft
setErrorMsg ft (Just (ExitFailure _,_,stdErr)) = ft {fileTreeErrorMsg = Just $ tshow stdErr}


-- | traverse the file tree entries, get updated content of each directory, and
-- add new files/remove deleted files from the FileTree.
refreshFileEntries :: Text -> [Entry FileTreeInfo]
                   -> IO ([FileInfo], [Entry FileTreeInfo])
                   -> IO ([FileInfo], [Entry FileTreeInfo])
refreshFileEntries indent [] accumulator = do
  (as, fes) <- accumulator
  case as of
    [] -> accumulator
    h:t -> refreshFileEntries indent [] (return (t,FileEntry (indent,h) : fes))

refreshFileEntries indent (fe@(FileEntry (_,fi)):entries) accumulator = do
  (as, fes) <- accumulator
  case as of
    [] -> accumulator
    h:t | h == fi -> refreshFileEntries indent entries (return (t,fe:fes))
    h:t -> refreshFileEntries indent (fe: entries) (return (t,FileEntry (indent,h) : fes))

refreshFileEntries indent (ed@(ExpandedDirectory (_,fi) dirEntries):entries) accumulator = do
  (as, fes) <- accumulator
  case as of
    [] -> accumulator
    h:t | h == fi -> do
      newDirEntries <- getDirectoryContent (fileInfoFilePath fi)
      (_, updatedDir) <- refreshFileEntries (indent <> "  ") dirEntries (return (newDirEntries, []))
      refreshFileEntries indent entries (return (t,ExpandedDirectory (indent,fi) updatedDir:fes))
    h:t -> refreshFileEntries indent (ed: entries) (return (t,FileEntry (indent, h) : fes))

refreshFileTree :: FileTree n -> IO (FileTree n)
refreshFileTree ft = do
  entries <- getDirectoryContent (fileTreeWorkingDirectory ft)
  let FileEntries fileInfoEntries = L.listElements $ fileTreeEntries ft
  (_,newEntries) <- refreshFileEntries "" fileInfoEntries (return (entries, []))
  return $ setEntries (FileEntries (reverse newEntries)) ft


-- | get contents of a directory to show in the file tree
getDirectoryContent :: FilePath -> IO [FileInfo]
getDirectoryContent workingDirectory = do
  entriesResult <- E.try $ FB.entriesForDirectory workingDirectory
  let (entries, _) = case entriesResult of
          Left (e::E.IOException) -> ([], Just e)
          Right es                -> (es, Nothing)
  return entries

-- | update the file tree to a new working directory
updateWorkingDirectory :: Config n -> FilePath -> IO (FileTree n)
updateWorkingDirectory conf workingDirectory = do
  entries <- getDirectoryContent workingDirectory
  let fileEntries = FileEntries (map fileInfoToEntry entries)
  return $ FileTree { fileTreeWorkingDirectory = workingDirectory
                    , fileTreeEntries = L.list (configName conf) fileEntries 1
                    , fileTreeAllEntries = fileEntries
                    , fileTreeMode = Normal
                    , fileTreeSearchText = ""
                    , fileTreeQuantifier = 1
                    , fileTreeErrorMsg = Nothing
                    , fileTreeConfig = conf
                    }

-- | Reload the file tree in the parent directory
goToParentDirectory :: FileTree n -> IO (FileTree n)
goToParentDirectory ft =
  updateWorkingDirectory (fileTreeConfig ft) (fileTreeWorkingDirectory ft </> "..")

-- | run the command on the file
runFile :: String -> FileInfo -> IO (Maybe (ExitCode, BS.ByteString, BS.ByteString))
runFile command fileInfo =
  case FB.fileStatusFileType <$> FB.fileInfoFileStatus fileInfo of
    Right (Just FB.RegularFile) -> do
      (ec,stdOut,stdErr) <- readProcess (shell (command ++ " " ++ FB.fileInfoFilePath fileInfo))
      BS.putStr stdOut                -- We want to make the errors printed to the users terminal
      BS.hPut stderr stdErr        -- for the user to further investigate
      return $ Just (ec,stdOut, stdErr)
    _ -> return Nothing

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
          M.suspendAndResume (runFile (fileTreeCommand ft) entry <&> setErrorMsg ft)

        Just FB.SymbolicLink  -> case fileInfoLinkTargetType entry of
            Just FB.Directory   ->
              M.suspendAndResume $ updateWorkingDirectory (fileTreeConfig ft)
                                                          (FB.fileInfoFilePath entry)
            Just FB.RegularFile ->
              M.suspendAndResume (runFile (fileTreeCommand ft) entry <&> setErrorMsg ft)
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
        FileEntry fi           -> ExpandedDirectory fi newDirectoryContents
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

          let newFileEntries = mapFileEntries (flipExpansion newDirectoryContents fileInfo)
                                 $ fileTreeAllEntries ft

          M.continue $ updateSearch $ mapEntries (const newFileEntries) ft

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
