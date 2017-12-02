module Main where

import Brainfuck.Parser
import Brainfuck.Interpreter
import System.IO             (withFile, hGetContents, IOMode(ReadMode), hGetBuffering, hSetBuffering, BufferMode(..), stdin)
import System.Environment    (getArgs)
import Control.Monad         (void)

main :: IO ()
main = do args <- getArgs
          if length args < 1
            then printHelp
            else executeBrainfuck $ head args

printHelp :: IO ()
printHelp = putStrLn $ "Execute brainfuck files." ++
    "\nUsage:" ++
    "\n\tRun file without input - brainfuck foo.bf" ++
    "\n\tRun file with input - echo ',++++.' | brainfuck foo.bf"

executeBrainfuck :: FilePath -> IO ()
executeBrainfuck fp = withFile fp ReadMode $ \handle ->
    do prevBuffering <- hGetBuffering stdin
       hSetBuffering stdin NoBuffering
       input <- hGetContents handle
       case parse input of
            Left err   -> print err
            Right cmds -> void $ evalCommands cmds initStack
       hSetBuffering stdin prevBuffering
