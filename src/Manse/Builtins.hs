-- | The built-in functions every Manse program starts with.
module Manse.Builtins (builtins) where

import Control.Monad (when)
import Control.Monad.IO.Class qualified as IO
import Control.Monad.Reader (asks)
import Data.Foldable (toList)
import Data.IORef
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Clock.POSIX (getPOSIXTime)
import Manse.Native
import Manse.Syntax
import Manse.Value hiding (display, repr)
import Manse.Value qualified as V
import System.Random (randomRIO)
import Prelude hiding (pure)
import Prelude qualified

-- Monomorphic versions, so the type of every built-in is fully determined and
-- 'native' can work out its arity and signature without annotations.
pure :: a -> Eval a
pure = Prelude.pure

liftIO :: IO a -> Eval a
liftIO = IO.liftIO

display, repr :: Value -> Eval Text
display = V.display
repr = V.repr

builtins :: [(Name, Value)]
builtins =
  [ native "pituus" $ \(x :: Either Text ManseList) -> case x of
      Left s -> pure (T.length s)
      Right (ManseList ref) -> Seq.length <$> liftIO (readIORef ref)
  , native "kello" (realToFrac <$> liftIO getPOSIXTime :: Eval Double)
  , native "lue" (asks ctxIn >>= liftIO :: Eval (Maybe Text))
  , native "numeroks" $ \(s :: Text) -> pure $ case TR.signed TR.rational (T.strip s) of
      Right (n, rest) | T.null rest -> Just (n :: Double)
      _ -> Nothing
  , native "tekstiks" $ \(v :: Value) -> display v
  , native "tyyppi" $ \(v :: Value) -> pure (typeName v)
  , native "kato" $ \(v :: Value) -> do
      out <- asks ctxOut
      repr v >>= liftIO . out . (<> "\n")
      pure v
  , native "lisää" $ \(ManseList ref) (v :: Value) -> liftIO (modifyIORef' ref (Seq.|> v))
  , native "poista" $ \(ManseList ref) -> do
      xs <- liftIO (readIORef ref)
      case Seq.viewr xs of
        Seq.EmptyR -> throwError "tyhjästä listasta ei voi poistaa mittään"
        rest Seq.:> x -> liftIO (writeIORef ref rest) >> pure x
  , native "kopio" $ \(ManseList ref) -> liftIO (readIORef ref)
  , native "väli" $ \(from :: Int) (to :: Int) ->
      pure (Seq.fromList [VNum (fromIntegral i) | i <- [from .. to - 1]])
  , native "satunnainen" $ \(lo :: Int) (hi :: Int) -> do
      when (lo > hi) $ throwError "satunnainen: alaraja on isompi ku yläraja"
      liftIO (randomRIO (lo, hi))
  , native "pyöristä" $ \(x :: Double) -> pure (fromInteger (floor (x + 0.5)) :: Double)
  , native "alaspäin" $ \(x :: Double) -> pure (fromInteger (floor x) :: Double)
  , native "ylöspäin" $ \(x :: Double) -> pure (fromInteger (ceiling x) :: Double)
  , native "itseisarvo" $ \(x :: Double) -> pure (abs x)
  , native "juuri" $ \(x :: Double) -> do
      when (x < 0) $ throwError "negatiivisesta luvusta ei saa juurta, ei ainakaa Tampereella"
      pure (sqrt x)
  , native "pilko" $ \(s :: Text) (sep :: Text) ->
      pure . Seq.fromList . map VStr $
        if T.null sep then map T.singleton (T.unpack s) else T.splitOn sep s
  , native "yhdistä" $ \(ManseList ref) (sep :: Text) -> do
      xs <- liftIO (readIORef ref)
      T.intercalate sep <$> mapM display (toList xs)
  , native "isoks" $ \(s :: Text) -> pure (T.toUpper s)
  , native "pieneks" $ \(s :: Text) -> pure (T.toLower s)
  , native "siistiks" $ \(s :: Text) -> pure (T.strip s)
  , native "osa" $ \(x :: Either Text ManseList) (from :: Int) (to :: Int) ->
      let slice :: (Int -> c -> c) -> (Int -> c -> c) -> Int -> c -> c
          slice tk dr len =
            let clamp i = max 0 (min len (if i < 0 then len + i else i))
                (a, b) = (clamp from, clamp to)
             in tk (max 0 (b - a)) . dr a
       in case x of
            Left s -> pure (VStr (slice T.take T.drop (T.length s) s))
            Right (ManseList ref) -> do
              xs <- liftIO (readIORef ref)
              newList (slice Seq.take Seq.drop (Seq.length xs) xs)
  , native "ohje" $ \(v :: Value) -> pure $ case v of
      VNative n | T.null (nativeSignature n) -> "<sisäänrakennettu roseduuri " <> nativeName n <> ">"
      VNative n -> nativeSignature n
      VFun c -> "roseduuri " <> funName (cloFun c) <> "(" <> T.intercalate ", " (funParams (cloFun c)) <> ")"
      VClass c -> "luakka " <> className c <> maybe "" ((" niinku " <>) . className) (classSuper c)
      _ -> "ei tästä oo ohjetta, tää on " <> typeName v
  , native "varmista" $ \(ok :: Value) (message :: Value) -> do
      when (not (isTruthy ok)) $ throwValue Nothing message
      pure ()
  ]
