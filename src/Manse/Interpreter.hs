-- | The tree-walking interpreter for resolved programs.
module Manse.Interpreter
  ( execProgram
  , eval
  , callValue
  , maxCallDepth
  ) where

import Control.Exception (try)
import Control.Monad (forM, forM_, unless, when, zipWithM_, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks, local)
import Data.Fixed (mod')
import Data.Foldable (toList)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Unique (newUnique)
import Manse.Syntax
import Manse.Value

-- | How a statement finished.
data Flow = Next | Returned Value | Broke | Continued

maxCallDepth :: Int
maxCallDepth = 100_000

execProgram :: Program Resolved -> Eval ()
execProgram (Program stmts) = mapM_ exec stmts

withEnv :: Env -> Eval a -> Eval a
withEnv env = local (\c -> c{ctxEnv = env})

-- | Run statements in order, stopping early on return, break or continue.
execBlock :: [Stmt Resolved] -> Eval Flow
execBlock = \case
  [] -> pure Next
  s : ss ->
    exec s >>= \case
      Next -> execBlock ss
      flow -> pure flow

bindName :: Bind -> Name -> Value -> Eval ()
bindName b name v = case b of
  BindLocal slot -> asks ctxEnv >>= \env -> writeLoc 0 slot env v
  BindGlobal -> defineGlobal name v

-- | Run a statement in a new one-slot frame holding @v@. Used for loop
-- variables and caught exceptions.
withBound :: Value -> Eval a -> Eval a
withBound v act = do
  frame <- asks ctxEnv >>= newFrame 1
  writeLoc 0 0 frame v
  withEnv frame act

exec :: Stmt Resolved -> Eval Flow
exec = \case
  ExprStmt e -> eval e >> pure Next
  VarDecl _ name b initial -> do
    v <- maybe (pure VNil) eval initial
    bindName b name v
    pure Next
  FunDecl b f -> do
    c <- makeClosure f
    bindName b (funName f) (VFun c)
    pure Next
  ClassDecl p name b super methods -> do
    superclass <- forM super $ \se ->
      eval se >>= \case
        VClass c -> pure c
        v -> throwErrorAt p ("luakka voi periä vaan toisen luakan, ei " <> typeName v)
    env <- asks ctxEnv
    methodEnv <- case superclass of
      Nothing -> pure env
      Just sc -> do
        frame <- newFrame 1 env
        writeLoc 0 0 frame (VClass sc)
        pure frame
    closures <- withEnv methodEnv (mapM makeClosure methods)
    u <- liftIO newUnique
    let cls = Class name superclass (M.fromList [(funName (cloFun c), c) | c <- closures]) u
    bindName b name (VClass cls)
    pure Next
  Print es -> do
    texts <- mapM (eval >=> display) es
    out <- asks ctxOut
    liftIO (out (T.unwords texts <> "\n"))
    pure Next
  Block 0 stmts -> execBlock stmts
  Block size stmts -> do
    frame <- asks ctxEnv >>= newFrame size
    withEnv frame (execBlock stmts)
  If c t e -> do
    v <- eval c
    if isTruthy v then exec t else maybe (pure Next) exec e
  Loop c body step ->
    let loop = do
          v <- eval c
          if not (isTruthy v)
            then pure Next
            else
              exec body >>= \case
                Broke -> pure Next
                Returned r -> pure (Returned r)
                _ -> forM_ step eval >> loop
     in loop
  ForEach p _ xs body -> do
    items <-
      eval xs >>= \case
        VList ref -> toList <$> liftIO (readIORef ref)
        VStr s -> pure (map (VStr . T.singleton) (T.unpack s))
        v -> throwErrorAt p ("jokaselle tarttee listan tai tekstin, mutta sai " <> typeName v)
    let loop = \case
          [] -> pure Next
          x : rest ->
            withBound x (exec body) >>= \case
              Broke -> pure Next
              Returned r -> pure (Returned r)
              _ -> loop rest
    loop items
  Return _ e -> Returned <$> maybe (pure VNil) eval e
  Break _ -> pure Broke
  Continue _ -> pure Continued
  Throw p e -> eval e >>= throwValue (Just p)
  Try body _ _ handler -> do
    ctx <- ask
    liftIO (try (runEval ctx (exec body))) >>= \case
      Right flow -> pure flow
      Left (ManseException v _ _) -> withBound v (exec handler)

makeClosure :: Fun Resolved -> Eval Closure
makeClosure f = Closure f <$> asks ctxEnv <*> liftIO newUnique

-- Expressions -----------------------------------------------------------------

eval :: Expr Resolved -> Eval Value
eval = \case
  Lit l -> pure $ case l of
    LNum n -> VNum n
    LStr s -> VStr s
    LBool b -> VBool b
    LNil -> VNil
  Interp parts ->
    VStr . T.concat <$> forM parts (\case Chunk t -> pure t; Splice e -> eval e >>= display)
  Var p name loc -> lookupVar p name loc
  Assign p name loc e -> do
    v <- eval e
    case loc of
      Local hops slot -> asks ctxEnv >>= \env -> writeLoc hops slot env v
      Global -> do
        ok <- setGlobal name v
        unless ok $
          throwErrorAt p ("\"" <> name <> "\" pitää esitellä ennen ku siihen voi sijottaa (seonnääs nii et " <> name <> " ...)")
    pure v
  Unary p op e -> eval e >>= unary p op
  Binary p op a b -> do
    x <- eval a
    y <- eval b
    binary p op x y
  Logical And a b -> eval a >>= \x -> if isTruthy x then eval b else pure x
  Logical Or a b -> eval a >>= \x -> if isTruthy x then pure x else eval b
  Call p f args -> do
    fv <- eval f
    vs <- mapM eval args
    callValue p fv vs
  Lambda f -> VFun <$> makeClosure f
  ListLit es -> mapM eval es >>= newList . Seq.fromList
  Index p a i -> do
    av <- eval a
    iv <- eval i
    index p av iv
  SetIndex p a i e -> do
    av <- eval a
    iv <- eval i
    v <- eval e
    case av of
      VList ref -> do
        xs <- liftIO (readIORef ref)
        ix <- checkIndex p iv (Seq.length xs)
        liftIO (writeIORef ref (Seq.update ix v xs))
        pure v
      _ -> throwErrorAt p ("ei voi sijottaa indeksiin: " <> typeName av <> " ei oo lista")
  Get p e name -> eval e >>= getProperty p name
  SetProp p e name ve -> do
    obj <- eval e
    v <- eval ve
    case obj of
      VInstance inst -> liftIO (modifyIORef' (instFields inst) (M.insert name v)) >> pure v
      _ -> throwErrorAt p ("vaan oliolla on kenttiä, " <> typeName obj <> " ei oo olio")
  This _ loc -> case loc of
    Local hops slot -> asks ctxEnv >>= readLoc hops slot
    Global -> error "resolver bug: itse resolved as global"
  Super p name loc -> case loc of
    Local hops slot -> do
      env <- asks ctxEnv
      superclass <- readLoc hops slot env
      inst <- readLoc (hops - 1) 0 env
      case superclass of
        VClass sc | Just m <- findMethod sc name -> VFun <$> bindMethod inst m
        _ -> throwErrorAt p ("kantaluakalla ei oo metodia \"" <> name <> "\"")
    Global -> error "resolver bug: kanta resolved as global"

lookupVar :: Pos -> Name -> Loc -> Eval Value
lookupVar p name = \case
  Local hops slot -> asks ctxEnv >>= readLoc hops slot
  Global ->
    getGlobal name >>= \case
      Just v -> pure v
      Nothing -> throwErrorAt p ("en tiiä mikä on \"" <> name <> "\"")

unary :: Pos -> UnOp -> Value -> Eval Value
unary p op v = case (op, v) of
  (Negate, VNum n) -> pure (VNum (negate n))
  (Negate, _) -> throwErrorAt p ("miinusmerkki toimii vaan luvuille, ei " <> typeName v)
  (Not, _) -> pure (VBool (not (isTruthy v)))

binary :: Pos -> BinOp -> Value -> Value -> Eval Value
binary p op x y = case (op, x, y) of
  (Add, VNum a, VNum b) -> num (a + b)
  (Add, VStr a, _) -> VStr . (a <>) <$> display y
  (Add, _, VStr b) -> VStr . (<> b) <$> display x
  (Add, VList a, VList b) -> do
    as <- liftIO (readIORef a)
    bs <- liftIO (readIORef b)
    newList (as <> bs)
  (Sub, VNum a, VNum b) -> num (a - b)
  (Mul, VNum a, VNum b) -> num (a * b)
  (Mul, VStr s, VNum n) -> VStr . (`T.replicate` s) <$> toInt n
  (Div, VNum _, VNum 0) -> throwErrorAt p "nollalla ei voi jakaa, hölömö"
  (Div, VNum a, VNum b) -> num (a / b)
  (Mod, VNum _, VNum 0) -> throwErrorAt p "nollalla ei voi jakaa, ei edes jakojäännöstä"
  (Mod, VNum a, VNum b) -> num (a `mod'` b)
  (Pow, VNum a, VNum b) -> num (a ** b)
  (Eq, _, _) -> VBool <$> valuesEqual x y
  (Neq, _, _) -> VBool . not <$> valuesEqual x y
  (_, VNum a, VNum b) | Just cmp <- comparison -> pure (VBool (cmp a b))
  (_, VStr a, VStr b) | Just cmp <- comparison -> pure (VBool (cmp a b))
  _ ->
    throwErrorAt p $
      "ei onnistu: " <> typeName x <> " " <> binOpName op <> " " <> typeName y
 where
  num = pure . VNum
  toInt n
    | n >= 0 && n == fromInteger (round n) = pure (round n)
    | otherwise = throwErrorAt p "tekstiä voi kertoa vaan ei-negatiivisella kokonaisluvulla"
  comparison :: Ord a => Maybe (a -> a -> Bool)
  comparison = case op of
    Gt -> Just (>)
    Gte -> Just (>=)
    Lt -> Just (<)
    Lte -> Just (<=)
    _ -> Nothing

-- | Validate an index, counting negative indices from the end.
checkIndex :: Pos -> Value -> Int -> Eval Int
checkIndex p iv len = case iv of
  VNum n | n == fromInteger (round n) -> do
    let i = round n
        i' = if i < 0 then len + i else i
    when (i' < 0 || i' >= len) $
      throwErrorAt p ("indeksi " <> showNumber n <> " ei mahu mukaan, pituus on " <> T.pack (show len))
    pure i'
  _ -> throwErrorAt p ("indeksin pitää olla kokonaisluku, ei " <> typeName iv)

index :: Pos -> Value -> Value -> Eval Value
index p av iv = case av of
  VList ref -> do
    xs <- liftIO (readIORef ref)
    ix <- checkIndex p iv (Seq.length xs)
    pure (Seq.index xs ix)
  VStr s -> do
    ix <- checkIndex p iv (T.length s)
    pure (VStr (T.singleton (T.index s ix)))
  _ -> throwErrorAt p ("ei voi indeksoida: " <> typeName av <> " ei oo lista eikä teksti")

-- | @olio.nimi@. Instances look up fields, then methods. For any other value,
-- @x.f(a, b)@ means @f(x, a, b)@, so @[1, 2, 3].kartota(tuplaa)@ works.
getProperty :: Pos -> Name -> Value -> Eval Value
getProperty p name obj = case obj of
  VInstance inst -> do
    fields <- liftIO (readIORef (instFields inst))
    case M.lookup name fields of
      Just v -> pure v
      Nothing -> case findMethod (instClass inst) name of
        Just m -> VFun <$> bindMethod obj m
        Nothing -> throwErrorAt p (className (instClass inst) <> "-oliolla ei oo mittään nimeltä \"" <> name <> "\"")
  _ ->
    getGlobal name >>= \case
      Just f | Just arity <- arityOf f, arity >= 1 ->
        pure . VNative $
          NativeFn
            { nativeName = name
            , nativeArity = arity - 1
            , nativeSignature = ""
            , nativeCall = callValue p f . (obj :)
            }
      _ -> throwErrorAt p (typeName obj <> " ei tiiä mikä on \"" <> name <> "\"")

arityOf :: Value -> Maybe Int
arityOf = \case
  VFun c -> Just (length (funParams (cloFun c)))
  VNative n -> Just (nativeArity n)
  _ -> Nothing

bindMethod :: Value -> Closure -> Eval Closure
bindMethod inst m = do
  frame <- newFrame 1 (cloEnv m)
  writeLoc 0 0 frame inst
  pure m{cloEnv = frame}

-- | Call anything callable.
callValue :: Pos -> Value -> [Value] -> Eval Value
callValue p f args = case f of
  VFun c -> callClosure p c args
  VNative n -> do
    checkArity p (nativeName n) (nativeArity n) args
    atPos p (nativeCall n args)
  VClass cls -> do
    inst <- VInstance . Instance cls <$> liftIO (newIORef M.empty)
    case findMethod cls "alota" of
      Just initializer -> do
        bound <- bindMethod inst initializer
        callClosure p bound args
      Nothing -> checkArity p (className cls) 0 args >> pure inst
  _ -> throwErrorAt p ("ei voi kutsua: " <> typeName f <> " ei oo roseduuri")

checkArity :: Pos -> Name -> Int -> [Value] -> Eval ()
checkArity p name expected args =
  when (length args /= expected) $
    throwErrorAt p $
      name <> " ottaa " <> T.pack (show expected) <> " argumenttia, mutta sille annettiin " <> T.pack (show (length args))

callClosure :: Pos -> Closure -> [Value] -> Eval Value
callClosure p (Closure f env _) args = do
  checkArity p (funName f) (length (funParams f)) args
  depth <- asks ctxDepth
  when (depth >= maxCallDepth) $
    throwErrorAt p "pino täynnä! liian syvä rekursio, ny riittää"
  frame <- newFrame (funFrame f) env
  zipWithM_ (\slot v -> writeLoc 0 slot frame v) [0 ..] args
  let enter c = c{ctxEnv = frame, ctxDepth = depth + 1, ctxStack = CallFrame (funName f) p : ctxStack c}
  flow <- local enter (execBlock (funBody f))
  case (funKind f, flow) of
    (InitializerKind, _) -> readLoc 0 0 env
    (_, Returned v) -> pure v
    _ -> pure VNil
