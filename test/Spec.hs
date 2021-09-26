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
runTests programs = 
    let getProgram = fromJust . (\n -> M.lookup (testCaseDir ++ n) programs) in 
    hspec $ do
    describe "e2e tests" $ do
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
            describe "variable scope" $ do
                let program = getProgram "scope.manse"
                it "can shadow variables in inner scope without affecting outer" $ do
                    execute (interpretProgram program [VBoolean True]) emptyEnv `shouldBe` Right (VNumber 0.0)
                it "can reassign variables in outer scope" $ do
                    execute (interpretProgram program [VBoolean False]) emptyEnv `shouldBe` Right (VNumber 1.0)
            describe "closures" $ do
                let program = getProgram "closure.manse"
                it "first-class-functions can access the environment they were defined in" $ do
                    execute (interpretProgram program []) emptyEnv `shouldBe` Right (VNumber 6.0)
    describe "parser tests" $ do
            describe "variable declaration" $ do
                it "without initializer" $ do
                    parseTextToProgram "seonnääs nii et a;" `shouldBe` Right (Program [D $ Declaration "a" Nothing])
                it "with initializer (simple)" $ do
                    parseTextToProgram "seonnääs nii et a on ny 3;" `shouldBe` Right (Program [D $ Declaration "a" (Just $ NumberLit 3.0)])
                it "with initializer (complex)" $ do
                    parseTextToProgram "seonnääs nii et a on ny b plus 3 kertaa c;" `shouldBe`
                        Right (Program [D $ Declaration "a" (Just $ Add (Ident "b") (Multiply (NumberLit 3.0) (Ident "c")))])
            describe "function declaration" $ do
                it "without params" $ do
                    parseTextToProgram "roseduuri foo() {}" `shouldBe` Right (Program [Func $ FunctionDeclaration "foo" [] $ BlockStatement []])
                it "with one param" $ do
                    parseTextToProgram "roseduuri foo(a) {}" `shouldBe` Right (Program [Func $ FunctionDeclaration "foo" ["a"] $ BlockStatement []])
                it "with multiple params" $ do
                    parseTextToProgram "roseduuri foo(a, b) {}" `shouldBe` Right (Program [Func $ FunctionDeclaration "foo" ["a", "b"] $ BlockStatement []])
                it "with block statement body" $ do
                    parseTextToProgram "roseduuri foo() { a on ny 3; kylä lähtee a plus 3; }" `shouldBe` 
                        Right (Program [Func $ FunctionDeclaration "foo" [] $
                            BlockStatement [
                                E $ ExpStatement $ Assign (Ident "a") (NumberLit 3.0),
                                Return $ ReturnStatement $ Just (Add (Ident "a") (NumberLit 3.0))
                            ]
                        ])
            describe "if statement" $ do
                it "without else block" $ do
                    parseTextToProgram "jos (tosi) ni {}" `shouldBe`
                        Right (Program [If $ IfStatement (BoolLit True) (BlockStatement []) Nothing])
                it "with else block" $ do
                    parseTextToProgram "jos (tosi) ni {} mut jos ei ni {}" `shouldBe`
                        Right (Program [If $ IfStatement (BoolLit True) (BlockStatement []) (Just (BlockStatement []))])
                it "with else block, and statements inside the blocks" $ do
                    parseTextToProgram "jos (tosi) ni { kylä lähtee 1; } mut jos ei ni { kylä lähtee 2; }" `shouldBe`
                        Right (Program [If $ IfStatement (BoolLit True)
                        (BlockStatement [Return $ ReturnStatement $ Just $ NumberLit 1.0 ])
                        (Just $ BlockStatement [Return $ ReturnStatement $ Just $ NumberLit 2.0 ])
                        ])
            describe "while statement" $ do
                it "condition and block statement" $ do
                    parseTextToProgram "kuha (a on isompi ku 0) ni { a on ny a miinus 1; }" `shouldBe`
                        Right (Program [While $ WhileStatement (CompareGT (Ident "a") (NumberLit 0.0))
                        (BlockStatement [E $ ExpStatement $ Assign (Ident "a") (Subtract (Ident "a") (NumberLit 1.0))])
                        ])
            describe "for statement" $ do
                it "with only initializing variable declaration" $ do
                    parseTextToProgram "elikkä jos (seonnääs nii et b on ny 0;) ni {}" `shouldBe`
                        Right (Program [For $ ForStatement (Left $ Declaration "b" $ Just (NumberLit 0.0)) Nothing Nothing (BlockStatement [])])
                it "with only initializing expression" $ do
                    parseTextToProgram "elikkä jos (b on ny 0;) ni {}" `shouldBe`
                        Right (Program [For $ ForStatement (Right $ ExpStatement $ Assign (Ident "b") (NumberLit 0.0)) Nothing Nothing (BlockStatement [])])
                it "with initializer and condition" $ do
                    parseTextToProgram "elikkä jos (seonnääs nii et b on ny 0; b om piänempi ku 5) ni {}" `shouldBe`
                        Right (Program [
                            For $ ForStatement
                                    (Left $ Declaration "b" $ Just (NumberLit 0.0))
                                    (Just (CompareLT (Ident "b") (NumberLit 5.0)))
                                    Nothing 
                                    (BlockStatement [])])
                it "with initializer, condition and incrementor" $ do
                    parseTextToProgram "elikkä jos (seonnääs nii et b on ny 0; b om piänempi ku 5; b on ny b plus 1) ni {}" `shouldBe`
                        Right (Program [
                            For $ ForStatement
                                    (Left $ Declaration "b" $ Just (NumberLit 0.0))
                                    (Just (CompareLT (Ident "b") (NumberLit 5.0)))
                                    (Just (Assign (Ident "b") (Add (Ident "b") (NumberLit 1.0))))
                                    (BlockStatement [])])


main :: IO ()
main = do
    testfiles <- map (testCaseDir ++) <$> listDirectory testCaseDir
    programs <- M.fromList . zip testfiles . map unsafeFromRight <$> mapM parseFile testfiles
    runTests programs
