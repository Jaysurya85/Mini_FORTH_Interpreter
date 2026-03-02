module Main where

import Interpret
import System.Environment

main :: IO ()
main = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  let (stack, output) = interpret contents

  putStrLn output

  if not (null stack)
    then do
      putStrLn "Warning: Stack not empty."
      print stack
    else return ()
