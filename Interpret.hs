module Interpret where

import qualified Data.Map as M
import Eval
import Val

-- Interpreter state:
-- (stack, output, definitions)
type State = ([Val], String, M.Map String [Val])

-- Main interpret function
interpret :: String -> ([Val], String)
interpret text =
  let tokens = map strToVal (words text)
      (stack, out, _) = run tokens ([], "", M.empty)
   in (stack, out)

-- Recursive execution engine
run :: [Val] -> State -> State
run [] state = state
-- Function definition start
run (Id ":" : Id name : rest) (stack, out, defs) =
  let (body, remaining) = collectBody rest []
      newDefs = M.insert name body defs
   in run remaining (stack, out, newDefs)
-- Execute word or operator
run (Id word : rest) (stack, out, defs) =
  case M.lookup word defs of
    Just body ->
      run (body ++ rest) (stack, out, defs)
    Nothing ->
      let (newStack, newOut) = evalOut word (stack, out)
       in run rest (newStack, newOut, defs)
-- Push literal onto stack
run (val : rest) (stack, out, defs) =
  run rest (val : stack, out, defs)

-- Collect function body until ';'
collectBody :: [Val] -> [Val] -> ([Val], [Val])
collectBody [] _ = error "Missing ';' in function definition"
collectBody (Id ";" : rest) acc = (reverse acc, rest)
collectBody (x : xs) acc = collectBody xs (x : acc)
