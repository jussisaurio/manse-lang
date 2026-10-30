-- | Runtime values, environments and the evaluation monad.
module Manse.Value
  ( -- * Values
    Value (..)
  , Closure (..)
  , NativeFn (..)
  , Class (..)
  , Instance (..)
  , typeName
  , isTruthy
  , display
  , repr
  , showNumber
  , valuesEqual
  , newList
  , findMethod

    -- * Environments
  , Env (..)
  , newFrame
  , readLoc
  , writeLoc

    -- * The evaluation monad
  , Eval (..)
  , Ctx (..)
  , CallFrame (..)
  , runEval
  , ManseException (..)
  , throwValue
  , throwError
  , throwErrorAt
  , atPos
  , getGlobal
  , setGlobal
  , defineGlobal
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Foldable (toList)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (Unique)
import Data.Vector.Mutable qualified as MV
import Manse.Syntax

-- Values ----------------------------------------------------------------------

data Value
  = VNum Double
  | VStr Text
  | VBool Bool
  | VNil
  | VList (IORef (Seq Value))
  | VFun Closure
  | VNative NativeFn
  | VClass Class
  | VInstance Instance

data Closure = Closure
  { cloFun :: Fun Resolved
  , cloEnv :: Env
  , cloId :: Unique
  }

data NativeFn = NativeFn
  { nativeName :: Name
  , nativeArity :: Int
  , nativeSignature :: Text
  , nativeCall :: [Value] -> Eval Value
  }

data Class = Class
  { className :: Name
  , classSuper :: Maybe Class
  , classMethods :: M.Map Name Closure
  , classId :: Unique
  }

data Instance = Instance
  { instClass :: Class
  , instFields :: IORef (M.Map Name Value)
  }

typeName :: Value -> Text
typeName = \case
  VNum _ -> "luku"
  VStr _ -> "teksti"
  VBool _ -> "totuusarvo"
  VNil -> "tyhjä"
  VList _ -> "lista"
  VFun _ -> "roseduuri"
  VNative _ -> "roseduuri"
  VClass _ -> "luakka"
  VInstance i -> className (instClass i)

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  _ -> True

showNumber :: Double -> Text
showNumber n
  | isNaN n = "eioolukukaan"
  | isInfinite n = if n > 0 then "ääretön" else "miinus ääretön"
  | n == fromInteger r && abs n < 1e15 = T.pack (show r)
  | otherwise = T.pack (show n)
 where
  r = round n :: Integer

-- | How a value looks when printed with @tulosta@.
display :: MonadIO m => Value -> m Text
display = \case
  VStr s -> pure s
  v -> repr v

-- | How a value looks inside a list, or in the REPL: strings get quotes.
repr :: MonadIO m => Value -> m Text
repr = \case
  VNum n -> pure (showNumber n)
  VStr s -> pure (T.pack (showString s))
  VBool True -> pure "tosi"
  VBool False -> pure "epätosi"
  VNil -> pure "tyhjä"
  VList ref -> do
    xs <- liftIO (readIORef ref)
    items <- mapM repr (toList xs)
    pure ("[" <> T.intercalate ", " items <> "]")
  VFun c -> pure ("<roseduuri " <> funName (cloFun c) <> ">")
  VNative n -> pure ("<sisäänrakennettu roseduuri " <> nativeName n <> ">")
  VClass c -> pure ("<luakka " <> className c <> ">")
  VInstance i -> do
    fields <- liftIO (readIORef (instFields i))
    shown <- mapM (\(k, v) -> ((k <> ": ") <>) <$> repr v) (M.toList fields)
    pure (className (instClass i) <> " { " <> T.intercalate ", " shown <> " }")
 where
  -- Like 'show' but without escaping non-ASCII. This is a Finnish language, after all.
  showString s = "\"" <> concatMap esc (T.unpack s) <> "\""
  esc = \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\t' -> "\\t"
    c -> [c]

-- | Structural equality for plain data and lists, identity for everything else.
valuesEqual :: MonadIO m => Value -> Value -> m Bool
valuesEqual a b = case (a, b) of
  (VNum x, VNum y) -> pure (x == y)
  (VStr x, VStr y) -> pure (x == y)
  (VBool x, VBool y) -> pure (x == y)
  (VNil, VNil) -> pure True
  (VList x, VList y)
    | x == y -> pure True
    | otherwise -> do
        xs <- liftIO (readIORef x)
        ys <- liftIO (readIORef y)
        if length xs /= length ys
          then pure False
          else and <$> sequence (zipWith valuesEqual (toList xs) (toList ys))
  (VFun x, VFun y) -> pure (cloId x == cloId y)
  (VNative x, VNative y) -> pure (nativeName x == nativeName y)
  (VClass x, VClass y) -> pure (classId x == classId y)
  (VInstance x, VInstance y) -> pure (instFields x == instFields y)
  _ -> pure False

newList :: MonadIO m => Seq Value -> m Value
newList xs = VList <$> liftIO (newIORef xs)

findMethod :: Class -> Name -> Maybe Closure
findMethod cls name = case M.lookup name (classMethods cls) of
  Just m -> Just m
  Nothing -> classSuper cls >>= (`findMethod` name)

-- Environments ----------------------------------------------------------------

-- | A chain of fixed-size frames. The resolver has already decided which frame
-- and slot every local variable lives in.
data Env = Frame (MV.IOVector Value) Env | TopLevel

newFrame :: MonadIO m => Int -> Env -> m Env
newFrame size parent = liftIO $ do
  slots <- MV.replicate size VNil
  pure (Frame slots parent)

ancestor :: Int -> Env -> MV.IOVector Value
ancestor hops env = case (hops, env) of
  (0, Frame slots _) -> slots
  (n, Frame _ parent) -> ancestor (n - 1) parent
  (_, TopLevel) -> error "resolver bug: walked off the top of the environment"

readLoc :: MonadIO m => Int -> Int -> Env -> m Value
readLoc hops slot env = liftIO (MV.unsafeRead (ancestor hops env) slot)

writeLoc :: MonadIO m => Int -> Int -> Env -> Value -> m ()
writeLoc hops slot env v = liftIO (MV.unsafeWrite (ancestor hops env) slot v)

-- The evaluation monad --------------------------------------------------------

data CallFrame = CallFrame {frameName :: Name, framePos :: Pos}

data Ctx = Ctx
  { ctxGlobals :: IORef (M.Map Name Value)
  , ctxEnv :: Env
  , ctxStack :: [CallFrame]
  , ctxDepth :: Int
  , ctxPos :: Maybe Pos
  -- ^ where we are, roughly, for errors raised deep inside built-ins
  , ctxOut :: Text -> IO ()
  , ctxIn :: IO (Maybe Text)
  }

newtype Eval a = Eval {unEval :: Ctx -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Ctx) via ReaderT Ctx IO

runEval :: Ctx -> Eval a -> IO a
runEval ctx m = unEval m ctx

-- | A Manse exception in flight: the thrown value, where, and the call stack.
data ManseException = ManseException Value (Maybe Pos) [CallFrame]

instance Show ManseException where
  show (ManseException _ p _) = "ManseException at " <> show p

instance Exception ManseException

throwValue :: Maybe Pos -> Value -> Eval a
throwValue p v = do
  stack <- asks ctxStack
  here <- asks ctxPos
  liftIO (throwIO (ManseException v (maybe here Just p) stack))

-- | Raise a runtime error with the given message at the current position.
throwError :: Text -> Eval a
throwError = throwValue Nothing . VStr

throwErrorAt :: Pos -> Text -> Eval a
throwErrorAt p = throwValue (Just p) . VStr

-- | Remember the position for errors raised further down.
atPos :: Pos -> Eval a -> Eval a
atPos p = local (\c -> c{ctxPos = Just p})

getGlobal :: Name -> Eval (Maybe Value)
getGlobal name = do
  ref <- asks ctxGlobals
  M.lookup name <$> liftIO (readIORef ref)

setGlobal :: Name -> Value -> Eval Bool
setGlobal name v = do
  ref <- asks ctxGlobals
  liftIO $ atomicModifyIORef' ref $ \m ->
    if M.member name m then (M.insert name v m, True) else (m, False)

defineGlobal :: Name -> Value -> Eval ()
defineGlobal name v = do
  ref <- asks ctxGlobals
  liftIO (modifyIORef' ref (M.insert name v))
