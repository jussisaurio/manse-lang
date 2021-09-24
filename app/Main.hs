module Main where

import Lib
import System.Environment
import System.Directory
import qualified Data.Text as T
import Text.Megaparsec (parse)

runFile :: IO ()
runFile = do
    args <- getArgs
    case safeHead args of
      Nothing -> print "no filename provided"
      Just fn -> runProgram fn []

data Testcase = Testcase { path :: String, args :: [RuntimeValue], expect :: RuntimeValue }

testcases :: [Testcase]
testcases = [
    Testcase { path = "./src/testcases/emptymain.manse", args = [], expect = VNil }
    , Testcase { path = "./src/testcases/for.manse", args = [VNumber 3], expect = VNumber 33.0 }
    , Testcase { path = "./src/testcases/while.manse", args = [VNumber 3], expect = VNumber 4.0 }
    , Testcase { path = "./src/testcases/viponassi.manse", args = [VNumber 2, VNumber 4], expect = VNumber 240.0 }
    ]

runTest tc = do
    program <- parseFile (path tc)
    case program of
      Left _ -> print "Failed to parse file, lolbal nice error message bro"
      Right p -> case runProgramWithArgs p (args tc) of
        Left err -> print (show err)
        Right rv -> if rv == expect tc
            then print (path tc <> "- expect " <> show (expect tc) <> " OK")
            else print (path tc <> "- expect " <> show (expect tc) <> " NOK, got " <> show rv)


runTests :: IO ()
runTests = do
    mapM_ runTest testcases

main = runTests
