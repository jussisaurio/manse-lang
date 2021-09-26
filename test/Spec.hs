import Lib

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Text.Megaparsec (parse)
import System.Directory

unsafeFromRight = either undefined id

testCaseDir = "./test/testcases/"

emptyEnv = Environment { parent = Nothing, variables = M.empty }

runTests :: M.Map String Program -> IO ()
runTests programs = hspec $ 
    describe "e2e tests" $ do
        let getProgram = fromJust . (\n -> M.lookup (testCaseDir ++ n) programs) in do
            describe "empty main function" $ do
                let program = getProgram "emptymain.manse"
                it "returns nil" $ do
                    execute (interpretProgram program []) emptyEnv `shouldBe` Right VNil
            describe "for loop" $ do
                let program = getProgram "for.manse"
                it "initializer, condition and incrementor work correctly" $ do
                    execute (interpretProgram program [VNumber 5.0]) emptyEnv `shouldBe` Right (VNumber 35.0)
            describe "while loop" $ do
                let program = getProgram "while.manse"
                it "looping while condition is met works correctly" $ do
                    execute (interpretProgram program [VNumber 5.0]) emptyEnv `shouldBe` Right (VNumber 6.0)
            describe "function call" $ do
                let program = getProgram "functioncall.manse"
                it "returning the sum of two function invocations in main" $ do
                    execute (interpretProgram program []) emptyEnv `shouldBe` Right (VNumber 22.0)
            describe "if/else" $ do
                let program = getProgram "ifelse.manse"
                it "if statements work with and without else block" $ do
                    execute (interpretProgram program [VNil]) emptyEnv `shouldBe` Right (VNumber 1.0)
                    execute (interpretProgram program [VNumber 5.0]) emptyEnv `shouldBe` Right (VNumber 5.0)
                    execute (interpretProgram program [VNumber 6.0]) emptyEnv `shouldBe` Right (VNumber 60.0)

main :: IO ()
main = do
    testfiles <- map (testCaseDir ++) <$> listDirectory testCaseDir
    programs <- M.fromList . zip testfiles . map unsafeFromRight <$> mapM parseFile testfiles
    runTests programs
