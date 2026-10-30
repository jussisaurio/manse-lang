{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Encoding (setLocaleEncoding)
import Manse
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode (..))
import System.IO

main :: IO ()
main = do
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  getArgs >>= \case
    [] -> repl
    [flag] | flag `elem` ["-h", "--help", "--apua"] -> usage
    ["-e", code] -> runProgram (Source "<komentorivi>" (T.pack code)) []
    file : args ->
      try (T.readFile file) >>= \case
        Left (e :: IOException) -> do
          hPutStrLn stderr ("Ei saatu tiedostoa auki: " <> show e)
          exitFailure
        Right code -> runProgram (Source file code) (map T.pack args)

usage :: IO ()
usage =
  putStr . unlines $
    [ "Käyttö:"
    , "  manse                     käynnistää REPLin"
    , "  manse TIEDOSTO [ARG...]   ajaa tiedoston; pää(args) saa argumentit listana"
    , "  manse -e KOODI            ajaa koodinpätkän"
    ]

readLine :: IO (Maybe Text)
readLine = do
  eof <- isEOF
  if eof then pure Nothing else Just <$> T.getLine

newStdInterpreter :: IO Interpreter
newStdInterpreter = newInterpreter (\t -> T.putStr t >> hFlush stdout) readLine

orDie :: IO (Either ManseError a) -> IO a
orDie act =
  act >>= \case
    Right a -> pure a
    Left e -> do
      renderError e >>= T.hPutStrLn stderr
      exitWith (ExitFailure 1)

runProgram :: Source -> [Text] -> IO ()
runProgram src args = do
  interp <- newStdInterpreter
  orDie (runSource interp src)
  _ <- orDie (callMain interp args)
  pure ()

-- REPL ------------------------------------------------------------------------

repl :: IO ()
repl = do
  interp <- newStdInterpreter
  home <- getHomeDirectory
  let settings = defaultSettings{historyFile = Just (home <> "/.manse_history")}
  putStrLn "Manse 1.0. Moro! Kirjota koodia, :q lopettaa."
  runInputT settings (loop interp (1 :: Int))
 where
  loop interp n = do
    getChunk "manse> " "" >>= \case
      Nothing -> outputStrLn "Moikka!"
      Just (T.strip -> code)
        | code `elem` [":q", ":quit", "heippa"] -> outputStrLn "Moikka!"
        | T.null code -> loop interp n
        | otherwise -> do
            let src = Source ("<repl " <> show n <> ">") code
            liftIO (replInput interp src) >>= \case
              Left e -> liftIO (renderError e) >>= outputStrLn . T.unpack
              Right (Just v) -> repr v >>= outputStrLn . T.unpack
              Right Nothing -> pure ()
            loop interp (n + 1)

  -- Keep reading lines while brackets are unbalanced, so blocks can span lines.
  getChunk prompt acc =
    getInputLine prompt >>= \case
      Nothing -> pure (if T.null acc then Nothing else Just acc)
      Just line -> do
        let acc' = acc <> T.pack line <> "\n"
        if depth acc' > 0 then getChunk "  ...> " acc' else pure (Just acc')

  depth = go (0 :: Int) False . T.unpack
   where
    go d _ [] = d
    go d True ('\\' : _ : cs) = go d True cs
    go d inStr ('"' : cs) = go d (not inStr) cs
    go d True (_ : cs) = go d True cs
    go d False (c : cs)
      | c `elem` ("({[" :: String) = go (d + 1) False cs
      | c `elem` (")}]" :: String) = go (d - 1) False cs
      | otherwise = go d False cs
