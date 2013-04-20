module Main
where

import Hoodoo.Fs
import Hoodoo.Todo
import Hoodoo.Actions
import System.Environment

main = do
  args <- getArgs
  contents <- getTodos
  let (showing, writing) = dispatch contents args
  length contents `seq` writeTodos (displayTodos writing)
  putStr $ displayTodos showing
