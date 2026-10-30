{-# LANGUAGE TemplateHaskell #-}

-- | The public face of the Manse interpreter: load code, run it, show errors.
module Manse
  ( Interpreter
  , newInterpreter
  , runSource
  , callMain
  , replInput
  , ManseError (..)
  , renderError
  , Source (..)
  , Value
  , repr
  , display
  ) where

import Control.Exception (try)
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.IORef (newIORef)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Manse.Builtins (builtins)
import Manse.Interpreter
import Manse.Parser
import Manse.Resolver
import Manse.Syntax
import Manse.Value

newtype Interpreter = Interpreter Ctx

data ManseError
  = ParseFailure ParseError
  | ResolveFailure [ResolveError]
  | RuntimeFailure ManseException

prelude :: Text
prelude = $(embedStringFile "prelude/prelude.manse")

-- | A fresh interpreter with the built-ins and the prelude loaded. Output goes
-- to the first function, input (for @lue()@) comes from the second.
newInterpreter :: (Text -> IO ()) -> IO (Maybe Text) -> IO Interpreter
newInterpreter out input = do
  globals <- newIORef (M.fromList builtins)
  let interp = Interpreter (Ctx globals TopLevel [] 0 Nothing out input)
  runSource interp (Source "<vakiokirjasto>" prelude) >>= \case
    Right () -> pure interp
    Left e -> renderError e >>= fail . T.unpack

compile :: Program Parsed -> Either ManseError (Program Resolved)
compile = first ResolveFailure . resolveProgram

runEvalSafely :: Interpreter -> Eval a -> IO (Either ManseError a)
runEvalSafely (Interpreter ctx) m = first RuntimeFailure <$> try (runEval ctx m)

-- | Run all top-level statements of a source file.
runSource :: Interpreter -> Source -> IO (Either ManseError ())
runSource interp src = case first ParseFailure (parseProgram src) >>= compile of
  Left e -> pure (Left e)
  Right program -> runEvalSafely interp (execProgram program)

-- | If the program defined @pää@, call it. A @pää@ with one parameter gets the
-- command line arguments as a list.
callMain :: Interpreter -> [Text] -> IO (Either ManseError Value)
callMain interp args = runEvalSafely interp $ do
  let here = Pos (Source startup "") 0
  getGlobal "pää" >>= \case
    Nothing -> pure VNil
    Just f@(VFun c) | length (funParams (cloFun c)) == 1 -> do
      argList <- newList (Seq.fromList (map VStr args))
      callValue here f [argList]
    Just f -> callValue here f []

-- | Run a line of REPL input. Bare expressions produce a value to print.
replInput :: Interpreter -> Source -> IO (Either ManseError (Maybe Value))
replInput interp src = case parseReplInput src of
  Left e -> pure (Left (ParseFailure e))
  Right (ReplStmts program) -> case compile program of
    Left e -> pure (Left e)
    Right resolved -> fmap (const Nothing) <$> runEvalSafely interp (execProgram resolved)
  Right (ReplExpr e) -> case compile (Program [ExprStmt e]) of
    Left err -> pure (Left err)
    Right (Program [ExprStmt resolved]) -> fmap Just <$> runEvalSafely interp (eval resolved)
    Right _ -> error "resolver changed the shape of the program"

-- Errors ----------------------------------------------------------------------

renderError :: ManseError -> IO Text
renderError = \case
  ParseFailure e -> pure (T.pack (renderParseError e))
  ResolveFailure es -> pure . T.intercalate "\n" $ [quote p msg | ResolveError p msg <- es]
  RuntimeFailure (ManseException v p stack) -> do
    msg <- display v
    let trace = [traceLine f | f <- stack]
    pure . T.intercalate "\n" $
      maybe ("Hei kato ny! " <> msg) (`quote` msg) p : trace
 where
  traceLine (CallFrame name cp)
    | srcName (posSource cp) == startup = "  roseduurissa " <> name
    | otherwise = "  roseduurissa " <> name <> ", jota kutsuttiin " <> location cp

startup :: FilePath
startup = "<käynnistys>"

-- | @file:line:col@
location :: Pos -> Text
location p = let (line, col, _) = lineCol p in T.pack (srcName (posSource p)) <> ":" <> tshow line <> ":" <> tshow col

-- | An error message with the offending line of source and a caret under it.
quote :: Pos -> Text -> Text
quote p msg =
  T.unlines
    [ "Hei kato ny! " <> location p <> ": " <> msg
    , gutter <> " |"
    , tshow line <> " | " <> code
    , gutter <> " | " <> T.replicate (col - 1) " " <> "^"
    ]
 where
  (line, col, code) = lineCol p
  gutter = T.replicate (T.length (tshow line)) " "

lineCol :: Pos -> (Int, Int, Text)
lineCol (Pos src off) = (line, col, code)
 where
  before = T.take off (srcText src)
  line = 1 + T.count "\n" before
  col = 1 + T.length (T.takeWhileEnd (/= '\n') before)
  code = case drop (line - 1) (T.lines (srcText src)) of
    l : _ -> l
    [] -> ""

tshow :: Show a => a -> Text
tshow = T.pack . show
