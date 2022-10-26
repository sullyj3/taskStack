{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment (getArgs)
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


data Command = Push Text | Peek | Pop | List | Clear


parseCommand :: [String] -> Maybe Command
parseCommand = \case
  [] -> Just Peek
  "push" : rest -> Just $ Push (T.unwords $ T.pack <$> rest)
  ["peek"] -> Just Peek
  ["clear"] -> Just Clear
  [c] | c `elem` ["pop", "done"] -> Just Pop
      | c `elem` ["list", "l"] -> Just List
  _ -> Nothing


main :: IO ()
main = do
  mcmd <- parseCommand <$> getArgs
  case mcmd of
    Nothing -> putStrLn "uhh what?"
    Just cmd -> handleCommand cmd
  where
    handleCommand = \case
      Push t -> push t
      Peek -> peek
      Pop -> pop
      List -> list
      Clear -> clear

-- TODO: tstk edit
-- TODO
-- we could have it search up the tree in case there's not a file in the current directory


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


peek :: IO ()
peek = do
  exitIfNoTaskFile
  ls <- T.lines <$> T.readFile taskStackFilePath
  case ls of
    -- file exists, but is empty
    [] -> alertNoTasks
    ls -> T.putStrLn $ last ls


-- Alerts the users and exists if task file is not present
exitIfNoTaskFile :: IO ()
exitIfNoTaskFile =
  unlessM (doesFileExist taskStackFilePath)
          (alertNoTaskFile >> exitFailure)


alertNoTasks = putStrLn "There are no tasks in the task stack!"
alertNoTaskFile = putStrLn $ "No `./" <> taskStackFilePath <>"` found in current directory."


pop :: IO ()
pop = do 
  exitIfNoTaskFile
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
  exitIfNoTaskFile
  contents <- readFile taskStackFilePath
  case contents of
    "" -> alertNoTasks
    _  -> putStr contents


clear :: IO ()
clear = do
  exitIfNoTaskFile
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
