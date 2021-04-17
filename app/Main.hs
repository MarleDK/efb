{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_efb

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_efb.version)
    (unlines
      [ "Executing File browser (efb) is a terminal UI file browser, which"
      , "executes a given COMMAND on"
      , "any selected file."
      ])
    "If you want to use a COMMAND with arguments, you should wrap it in single quotes ''."
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> argument str (metavar "COMMAND")
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
