module Hoodoo.Fs
( getTodos
, writeTodos
) where

import System.Directory
import System.IO.Error(catch)
import Control.Applicative

getTodos :: IO String
getTodos = do
  path <- getTodoFile
  exists <- doesFileExist path
  if exists
    then readFile path
    else do
      writeTodos ""
      getTodos

writeTodos :: String -> IO ()
writeTodos str = getTodoFile >>= \f -> writeFile f str

getTodoFile :: IO [Char]
getTodoFile = withTodo <$> getGit

getGit :: IO FilePath
getGit = checkDir getCurrentDirectory

checkDir :: IO FilePath -> IO FilePath
checkDir path = do
  has <- dirHasGit path
  if has
    then path
    else checkDir (upDir <$> path)

dirHasGit :: IO FilePath -> IO Bool
dirHasGit path = hasGit <$> (path >>= getDirectoryContents)

slash = (== '/')
stripLastSlash = reverse . dropWhile slash . reverse

appendTo path s = (stripLastSlash path) ++ s
upDir = (`appendTo` "/../")
withTodo = (`appendTo` "/TODO")

isGit = (== ".git")
hasGit xs = foldl (\a b -> if a then a else isGit b) False xs
