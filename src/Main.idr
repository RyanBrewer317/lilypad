module Main

import Grammar
import Parser
import Typechecker

main : IO ()
main = do
  let code = "data MaybeInt = None | Some Int\nfn main (n : Int) : MaybeInt = Some n"
  case parse parseTop code of
    Left (SyntaxError e) => putStrLn e
    Left _ => putStrLn "oh no..."
    Right stx => 
      case checkProgram stx of
        Left (TypeError e) => putStrLn e
        Left _ => putStrLn "ruh roh..."
        Right fun => putStrLn "yay!"
