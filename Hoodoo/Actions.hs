module Hoodoo.Actions

where

import Hoodoo.Todo

type Args = [String]

data Action = Action { triggers :: [String]
                     , fn :: ([Todo] -> Args -> ([Todo], [Todo]))
                     }

actions = [ (Action ["", "la"] listAll)
          , (Action ["list-done", "ld"] listDone)
          , (Action ["list-left", "lf"] listLeft)
          , (Action ["done", "d"] markDone)
          , (Action ["add", "a"] addTask)
          , (Action ["clean", "c"] clean)
          ]

actionName :: String -> Action -> Bool
actionName name action = elem name $ triggers action

findByName name = takeAction $ filter (actionName name) actions

takeAction :: [Action] -> Action
takeAction (x:[]) = x
takeAction [] = head actions


dispatch :: String -> Args -> ([Todo], [Todo])
dispatch content [] = dispatch content [""]
dispatch content (name:args) = (fn action) ts args
  where action = findByName name
        ts = map todoFromString $ lines content

listAll :: [Todo] -> Args -> ([Todo], [Todo])
listAll todos _ = (todos, todos)

listDone :: [Todo] -> Args -> ([Todo], [Todo])
listDone todos _ = (keepFinished todos, todos)

listLeft :: [Todo] -> Args -> ([Todo], [Todo])
listLeft todos _ = (removeFinished todos, todos)

addTask :: [Todo] -> Args -> ([Todo], [Todo])
addTask ts args = listAll todos args
  where todos = reorderTodos $ addTodo ts (head args)

markDone :: [Todo] -> Args -> ([Todo], [Todo])
markDone ts args = listAll todos args
  where todos = markAsFinished ts (read (head args) :: Int)

clean :: [Todo] -> Args -> ([Todo], [Todo])
clean ts args = listAll todos args
  where todos = reorderTodos $ removeFinished ts

