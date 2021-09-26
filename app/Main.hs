module Main where

import Lib
import System.Environment

runFile :: IO ()
runFile = do
    args <- getArgs
    case safeHead args of
      Nothing -> print "no filename provided"
      Just fn -> runProgram fn []

main = print "tba"
