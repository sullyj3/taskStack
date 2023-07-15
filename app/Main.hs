{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
module Main where
import System.Environment (getArgs, lookupEnv)
import Data.Foldable

import System.Directory

import System.Environment.XDG.BaseDir qualified as XDG
import System.Exit (exitSuccess, exitFailure)
import Data.List.Extra (unsnoc)
import System.IO
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Control.Monad
import Control.Monad.Extra (whenM, ifM, unlessM)
import System.Process (callCommand)
import Data.Maybe (fromMaybe)


data Command = Help | Push Text | Peek | Pop | List | Clear | Edit | BareInvocation


parseCommand :: [String] -> Maybe Command
parseCommand = \case
  [] -> Just BareInvocation
  "push" : rest -> Just $ Push (T.unwords $ T.pack <$> rest)
  ["peek"] -> Just Peek
  ["clear"] -> Just Clear
  [c] | c `elem` ["help", "--help"] -> Just Help
      | c `elem` ["pop", "done"] -> Just Pop
      | c `elem` ["list", "l"] -> Just List
      | c `elem` ["edit", "e"] -> Just Edit
  _ -> Nothing


main :: IO ()
main = do
  mcmd <- parseCommand <$> getArgs
  case mcmd of
    Nothing -> printUsage
    Just cmd -> handleCommand cmd
  where
    handleCommand = \case
      Help -> printUsage
      Push t -> push t
      Peek -> peek DontPrintUsage
      Pop -> pop
      List -> list
      Clear -> clear
      Edit -> edit
      BareInvocation -> peek PrintUsage

printUsage :: IO ()
printUsage = putStrLn $ 
  "Usage: tstk [push <task> | peek | pop | list | clear | edit]" <>
  "\n\n" <>
  "A bare invocation of `tstk` is equivalent to `tstk peek`."

edit :: IO ()
edit = do
  mEditor <- lookupEnv "EDITOR"
  let editor = fromMaybe "vi" mEditor
  callCommand $ editor <> " .taskStack"


taskStackFilePath :: FilePath
taskStackFilePath = ".taskStack"


push :: Text -> IO ()
push task 
  | T.null $ T.strip task = do 
      putStrLn "No task description provided!"
      exitFailure
  | otherwise = do
      unlessM (doesFileExist taskStackFilePath)
              (putStrLn $ 
                "No `./" <> taskStackFilePath <>"` found in current directory, creating one now.")
      T.appendFile taskStackFilePath $ task <> "\n"
      putStrLn "Task added."

-- whether to print usage if task file is not present
data ShouldPrintUsage = PrintUsage | DontPrintUsage
  deriving (Eq)

peek :: ShouldPrintUsage -> IO ()
peek shouldPrintUsage = do
  exitIfNoTaskFile shouldPrintUsage
  ls <- T.lines <$> T.readFile taskStackFilePath
  case ls of
    -- file exists, but is empty
    [] -> alertNoTasks
    ls -> T.putStrLn $ last ls

-- Alerts the users and exists if task file is not present
exitIfNoTaskFile :: ShouldPrintUsage -> IO ()
exitIfNoTaskFile shouldPrintUsage = do
  unlessM (doesFileExist taskStackFilePath)
          do
            alertNoTaskFile 
            when (shouldPrintUsage == PrintUsage) do
              putStrLn ""
              printUsage
            exitFailure


alertNoTasks = putStrLn "There are no tasks in the task stack!"
alertNoTaskFile = putStrLn $ 
  "No `./" <> taskStackFilePath <>"` found in current directory.\n" <>
  "To create one, run `tstk push <task>`, or `tstk edit` to edit it manually."


pop :: IO ()
pop = do 
  exitIfNoTaskFile DontPrintUsage
  ls <- T.lines <$> T.readFile taskStackFilePath
  case unsnoc ls of
    Nothing -> alertNoTasks
    Just (initLines, lastLine) -> do
      T.writeFile taskStackFilePath $ T.unlines initLines
      T.putStrLn $ "Task completed: " <> lastLine
      case initLines of
        [] -> do
          putStrLn $ "That was the last one, deleting `./" <> taskStackFilePath <> "`."
          removeFile taskStackFilePath
        (_:_) -> let newTop = last initLines
                  in T.putStrLn $ "Current task: " <> newTop

  
list :: IO ()
list = do
  exitIfNoTaskFile DontPrintUsage
  contents <- readFile taskStackFilePath
  case contents of
    "" -> alertNoTasks
    _  -> putStr contents


clear :: IO ()
clear = do
  exitIfNoTaskFile DontPrintUsage
  whenM shouldDelete $ removeFile taskStackFilePath
  where 
    shouldDelete = do
      isNonEmpty <- not . null <$> readFile taskStackFilePath
      if isNonEmpty
        then do
          putStr $ "Are you sure you want to delete `./" <> taskStackFilePath <> "`? (Y/n)"
          hFlush stdout
          line <- getLine
          pure (line `elem` ["Y","y",""])
        else pure True
