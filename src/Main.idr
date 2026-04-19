module Main

import System.File.ReadWrite

import Grammar
import Parser
import Typechecker

main : IO ()
main = do
  mbCode <- readFile "sample.llp"
  case mbCode of
    Left _ => putStrLn "couldn't read sample.llp"
    Right code =>
      case parse parseTop code of
        Left (SyntaxError e) => putStrLn e
        Left _ => putStrLn "oh no..."
        Right stx => 
          case checkProgram stx of
            Left (TypeError e) => putStrLn e
            Left _ => putStrLn "ruh roh..."
            Right fun => putStrLn "yay!"
