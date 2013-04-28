module Hoodoo.Fs
( getTodos
, writeTodos
) where

import System.Directory
import Control.Applicative

rootError = "Hoodoo is meant to be used in a git project" ++
            "and reached the root directory looking for it."

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
  root <- isPathRoot path
  if has
    then path
    else if root
      then error rootError
      else checkDir $ (upDir <$> path) >>= canonicalizePath

isPathRoot :: IO FilePath -> IO Bool
isPathRoot path = isRoot <$> path

dirHasGit :: IO FilePath -> IO Bool
dirHasGit path = hasGit <$> (path >>= getDirectoryContents)

-- Helpers

slash = (== '/')
stripLastSlash = reverse . dropWhile slash . reverse

appendTo path s = (stripLastSlash path) ++ s
upDir = (`appendTo` "/../")
withTodo = (`appendTo` "/TODO")

isGit = (== ".git")
hasGit xs = foldl (\a b -> if a then a else isGit b) False xs

isRoot = (== "/")

