module Hoodoo.Todo
( Todo
, todoFromString
, removeFinished
, keepFinished
, reorderTodos
, displayTodos
, markAsFinished
, removeTodo
, rerankTodo
, addTodo
) where

import qualified Data.List as List
import qualified Data.Text as Text
--
data Todo = Todo { position :: Int
                 , finished :: Bool
                 , task :: String
                 } deriving (Show)

displayFinished :: Todo -> String
displayFinished t = if finished t then "[x]" else "[ ]"

displayTodo :: Todo -> String
displayTodo t = (show $ position t) ++ ") " ++ displayFinished t ++ " " ++ task t

displayTodos :: [Todo] -> String
displayTodos ts = unlines $ map displayTodo ts

removeFinished :: [Todo] -> [Todo]
removeFinished ts = filter (not . finished) ts

keepFinished :: [Todo] -> [Todo]
keepFinished ts = filter finished ts

removeTodo :: [Todo] -> Int -> [Todo]
removeTodo ts pos = reorderTodos $ filter (\t -> position t /= pos) ts

markAsFinished :: [Todo] -> Int -> [Todo]
markAsFinished ts pos = reorderTodos $ map (\t -> if position t == pos then markFinished t else t) ts

rerankTodo :: Todo -> Int -> Todo
rerankTodo t pos = Todo {position=pos, finished=(finished t), task=(task t)}

markFinished :: Todo -> Todo
markFinished (Todo {finished=_, position=pos, task=t}) = Todo pos True t

reorderTodos :: [Todo] -> [Todo]
reorderTodos ts = map (\i -> rerankTodo (ts !! i) (succ i)) [0..count]
                where count = length ts - 1

addTodo :: [Todo] -> String -> [Todo]
addTodo ts t = reverse $ Todo {task=t, finished=False, position=0} : (reverse ts)

numeric :: Char -> Bool
numeric s = elem s ['0'..'9']

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack

todoFromString :: String -> Todo
todoFromString s = let num = read (takeWhile numeric s) :: Int
                       rest = trim $ drop 1 $ dropWhile numeric s
                       fin = elem 'x' $ take 3 rest
                       t = trim $ drop 3 rest
                   in Todo {position=num, finished=fin, task=t}
