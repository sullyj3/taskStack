{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> peek
    "push" : rest -> push (T.unwords $ T.pack <$> rest)
    ["peek"] -> peek
    ["pop"] -> pop
    ["done"] -> pop
    ["list"] -> list
    ["clear"] -> clear
    _ -> do
      putStrLn "uhh what?"

-- TODO: tstk edit
-- TODO: `tstk l` for list etc

-- TODO actually this might be more useful if it's directory local, rather than xdg?
-- that way you can have per project tasks you're working on, and come back and
-- re-establish context later
-- we could have it search up the tree in case there's not a file in the current directory
-- the last pop should delete the file, as should clear

-- produce the filepath to the tagstack file, ensuring that the file exists
ensureDataDirExists :: IO FilePath
ensureDataDirExists = do
  dirPath <- XDG.getUserDataDir "taskStack"
  createDirectoryIfMissing True dirPath
  pure dirPath

taskStackFilePath :: IO FilePath
taskStackFilePath = XDG.getUserDataFile "taskStack" "taskStack"

push :: Text -> IO ()
push task 
  | T.null $ T.strip task = do 
      putStrLn "No task description provided!"
      exitFailure
  | otherwise = do
      ensureDataDirExists
      fp <- taskStackFilePath
      T.appendFile fp $ task <> "\n"

alertNoTasks = putStrLn "There are no tasks in the task stack!"

-- todo handle no file
peek :: IO ()
peek = do
  fp <- taskStackFilePath
  ls <- T.lines <$> T.readFile fp

  case ls of
    -- file exists, but is empty
    [] -> alertNoTasks
    ls -> T.putStrLn $ last ls

-- todo handle no file
pop :: IO ()
pop = do 
  fp <- taskStackFilePath
  ls <- T.lines <$> T.readFile fp
  case unsnoc ls of
    Nothing -> alertNoTasks
    Just (initLines, lastLine) -> do
      T.writeFile fp $ T.unlines initLines
      T.putStrLn $ "Task completed: " <> lastLine
  
list :: IO ()
list = do
  fp <- taskStackFilePath
  contents <- readFile fp
  case contents of
    "" -> alertNoTasks
    _  -> putStr contents

clear :: IO ()
clear = do
  fp <- taskStackFilePath
  exists <- doesFileExist fp
  when exists $ do
    putStrLn $ "Are you sure you want to delete `" <> fp <> "`? (Y/n)"
    line <- getLine
    when (line `elem` ["Y","y",""]) $ 
      removeFile fp
