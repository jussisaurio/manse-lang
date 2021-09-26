import Lib

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Text.Megaparsec (parse)

unsafeFromRight = either undefined id

testfiles = [
    "./test/testcases/emptymain.manse",
    "./test/testcases/for.manse",
    "./test/testcases/while.manse",
    "./test/testcases/viponassi.manse"
    ]

emptyEnv = Environment { parent = Nothing, variables = M.empty }

runTests :: M.Map String Program -> IO ()
runTests programs = hspec $ 
    describe "e2e tests" $ do
        let getProgram = fromJust . (\n -> M.lookup n programs) in do
            describe "empty main function" $ do
                let program = getProgram "./test/testcases/emptymain.manse"
                it "returns nil" $ do
                    execute (interpretProgram program []) emptyEnv `shouldBe` Right VNil
            describe "for loop" $ do
                let program = getProgram "./test/testcases/for.manse"
                it "initializer, condition and incrementor work correctly" $ do
                    execute (interpretProgram program [VNumber 5.0]) emptyEnv `shouldBe` Right (VNumber 35.0)
            describe "while loop" $ do
                let program = getProgram "./test/testcases/while.manse"
                it "looping while condition is met works correctly" $ do
                    execute (interpretProgram program [VNumber 5.0]) emptyEnv `shouldBe` Right (VNumber 6.0)

main :: IO ()
main = do
    programs <- M.fromList . zip testfiles . map unsafeFromRight <$> mapM parseFile testfiles
    runTests programs
