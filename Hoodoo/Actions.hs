module Hoodoo.Actions where

import Hoodoo.Todo

type Args = [String]
type Handler = ([Todo] -> Args -> ([Todo], [Todo]))

data Action = Action { triggers :: [String], fn :: Handler }

actions = [ (Action ["", "la"] listAll)
          , (Action ["list-done", "ld"] listDone)
          , (Action ["list-left", "lf"] listLeft)
          , (Action ["done", "d"] markDone)
          , (Action ["add", "a"] addTask)
          , (Action ["clean", "c"] clean)
          ]

dispatch :: String -> Args -> ([Todo], [Todo])
dispatch content [] = dispatch content [""]
dispatch content (name:args) = (fn action) ts args
  where action = findByName name
        ts = map todoFromString $ lines content

listAll :: Handler
listAll todos _ = (todos, todos)

listDone :: Handler
listDone todos _ = (keepFinished todos, todos)

listLeft :: Handler
listLeft todos _ = (removeFinished todos, todos)

addTask :: Handler
addTask ts args = listAll todos args
  where todos = reorderTodos $ addTodo ts (head args)

markDone :: Handler
markDone ts args = listAll todos args
  where todos = markAsFinished ts (read (head args) :: Int)

clean :: Handler
clean ts args = listAll todos args
  where todos = reorderTodos $ removeFinished ts

-- Action helpers

actionName :: String -> Action -> Bool
actionName name action = elem name $ triggers action

findByName :: String -> Action
findByName name = takeAction $ filter (actionName name) actions

takeAction :: [Action] -> Action
takeAction (x:[]) = x
takeAction [] = head actions
