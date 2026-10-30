-- | Golden tests: every @test/golden/NAME.manse@ is run, and everything it prints
-- (including errors) is compared against @NAME.out@. Optional @NAME.in@ is fed
-- to @lue()@ and @NAME.args@ (one per line) is passed to @pää@.
--
-- Run with @GOLDEN_ACCEPT=1 stack test@ to (re)write the expected outputs.
module Main (main) where

import Control.Monad (forM_, when)
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Manse
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Test.Hspec

goldenDir :: FilePath
goldenDir = "test/golden"

-- | Run a program the way the CLI would, capturing all output.
runCaptured :: FilePath -> Text -> [Text] -> [Text] -> IO Text
runCaptured name code input args = do
  out <- newIORef []
  inputs <- newIORef input
  let emit t = modifyIORef' out (t :)
      readLine = atomicModifyIORef' inputs $ \case
        [] -> ([], Nothing)
        l : ls -> (ls, Just l)
  interp <- newInterpreter emit readLine
  result <- runSource interp (Source name code)
  case result of
    Left e -> renderError e >>= emit . (<> "\n")
    Right () ->
      callMain interp args >>= \case
        Left e -> renderError e >>= emit . (<> "\n")
        Right _ -> pure ()
  T.concat . reverse <$> readIORef out

readOptionalLines :: FilePath -> IO [Text]
readOptionalLines path = do
  exists <- doesFileExist path
  if exists then T.lines <$> T.readFile path else pure []

main :: IO ()
main = do
  accept <- isJust <$> lookupEnv "GOLDEN_ACCEPT"
  files <- sort . filter ((== ".manse") . takeExtension) <$> listDirectory goldenDir
  hspec . describe "golden" $
    forM_ files $ \file -> it file $ do
      let path = goldenDir </> file
          expectedPath = replaceExtension path "out"
      code <- T.readFile path
      input <- readOptionalLines (replaceExtension path "in")
      args <- readOptionalLines (replaceExtension path "args")
      actual <- runCaptured path code input args
      when accept $ T.writeFile expectedPath actual
      expected <- T.readFile expectedPath
      actual `shouldBe` expected
