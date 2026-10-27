{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Turning ordinary Haskell functions into Manse built-ins.
--
-- A built-in is written as a plain Haskell function over Haskell types:
--
-- > native "pituus" $ \(x :: Either Text ManseList) -> ...
--
-- and everything else is derived from its type: the arity (computed at the
-- type level by 'Arity'), the argument checks and their error messages, and
-- the human-readable signature that @ohje(pituus)@ shows.
module Manse.Native
  ( native
  , FromValue (..)
  , ToValue (..)
  , NativeSig (..)
  , Arity
  , ManseList (..)
  , Callable (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError, natVal, type (+))
import Manse.Syntax (Name)
import Manse.Value

-- | Arguments: Manse values that can be decoded into Haskell values.
class FromValue a where
  fromValue :: Value -> Maybe a

  -- | What the argument is called in signatures and error messages.
  expected :: Text

instance FromValue Value where
  fromValue = Just
  expected = "mikä vaan"

instance FromValue Double where
  fromValue = \case VNum n -> Just n; _ -> Nothing
  expected = "luku"

instance FromValue Int where
  fromValue = \case
    VNum n | n == fromInteger (round n) -> Just (round n)
    _ -> Nothing
  expected = "kokonaisluku"

instance FromValue Text where
  fromValue = \case VStr s -> Just s; _ -> Nothing
  expected = "teksti"

instance FromValue Bool where
  fromValue = \case VBool b -> Just b; _ -> Nothing
  expected = "totuusarvo"

-- | A reference to a mutable Manse list.
newtype ManseList = ManseList (IORef (Seq Value))

instance FromValue ManseList where
  fromValue = \case VList ref -> Just (ManseList ref); _ -> Nothing
  expected = "lista"

-- | Anything that can be called: functions, built-ins and classes.
newtype Callable = Callable Value

instance FromValue Callable where
  fromValue v = case v of
    VFun _ -> Just (Callable v)
    VNative _ -> Just (Callable v)
    VClass _ -> Just (Callable v)
    _ -> Nothing
  expected = "roseduuri"

-- | Either kind of argument is accepted.
instance (FromValue a, FromValue b) => FromValue (Either a b) where
  fromValue v = (Left <$> fromValue v) <|> (Right <$> fromValue v)
  expected = expected @a <> " tai " <> expected @b

-- | Results: Haskell values that can be turned back into Manse values.
class ToValue a where
  toValue :: a -> Eval Value
  returns :: Text

instance ToValue Value where
  toValue = pure
  returns = "mikä vaan"

instance ToValue Double where
  toValue = pure . VNum
  returns = "luku"

instance ToValue Int where
  toValue = pure . VNum . fromIntegral
  returns = "kokonaisluku"

instance ToValue Text where
  toValue = pure . VStr
  returns = "teksti"

instance ToValue Bool where
  toValue = pure . VBool
  returns = "totuusarvo"

instance ToValue () where
  toValue () = pure VNil
  returns = "tyhjä"

instance ToValue (Seq Value) where
  toValue xs = VList <$> liftIO (newIORef xs)
  returns = "lista"

instance ToValue a => ToValue (Maybe a) where
  toValue = maybe (pure VNil) toValue
  returns = returns @a <> " tai tyhjä"

-- | The number of arguments a native function takes, computed from its type.
-- Built-ins must end in 'Eval', and you get a friendly compile error if not.
type family Arity (f :: Type) :: Nat where
  Arity (a -> r) = 1 + Arity r
  Arity (Eval a) = 0
  Arity f =
    TypeError
      ( 'Text "Natiivin roseduurin pitää palauttaa (Eval jotain), mutta tää palauttaa "
          :<>: ShowType f
          :$$: 'Text "Kääri tulos pure:lla."
      )

-- | Function types that can act as Manse built-ins, peeled one argument at a time.
class NativeSig f where
  -- | Decode the arguments one by one (counting from 1 for error messages), then run.
  applyNative :: Name -> Int -> f -> [Value] -> Eval Value

  paramNames :: [Text]
  resultName :: Text

instance (FromValue a, NativeSig r) => NativeSig (a -> r) where
  applyNative name i f = \case
    v : vs -> case fromValue @a v of
      Just a -> applyNative name (i + 1) (f a) vs
      Nothing ->
        throwError $
          name <> ": " <> T.pack (show i) <> ". argumentin pitäis olla " <> expected @a
            <> ", mutta se on " <> typeName v
    [] -> error "native called with too few arguments, arity is checked before"
  paramNames = expected @a : paramNames @r
  resultName = resultName @r

instance ToValue a => NativeSig (Eval a) where
  applyNative _ _ m _ = m >>= toValue
  paramNames = []
  resultName = returns @a

-- | Wrap a Haskell function as a named Manse built-in.
native :: forall f. (NativeSig f, KnownNat (Arity f)) => Name -> f -> (Name, Value)
native name f =
  ( name
  , VNative
      NativeFn
        { nativeName = name
        , nativeArity = fromIntegral (natVal (Proxy @(Arity f)))
        , nativeSignature =
            name <> "(" <> T.intercalate ", " (paramNames @f) <> ") -> " <> resultName @f
        , nativeCall = applyNative name 1 f
        }
  )
