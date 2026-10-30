-- | Static scope resolution: turns @'Program' ''Parsed'@ into @'Program' ''Resolved'@.
--
-- Every local variable gets a fixed slot in a fixed frame, so at runtime a
-- variable lookup is "walk up N frames, index slot M" instead of a chain of
-- map lookups. It also catches mistakes that are visible without running the
-- program, like @kylä lähtee@ outside a function.
module Manse.Resolver
  ( resolveProgram
  , ResolveError (..)
  ) where

import Control.Monad (forM, unless, when, zipWithM_)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.List (findIndex)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Manse.Syntax

data ResolveError = ResolveError Pos Text
  deriving (Show)

-- | Names declared in a scope (with their slot, and whether their initializer
-- has finished), and the number of slots used so far.
data Scope = Scope (M.Map Name (Int, Bool)) Int

scopeNames :: Scope -> M.Map Name (Int, Bool)
scopeNames (Scope names _) = names

data ClassCtx = NoClass | InClass | InSubclass
  deriving (Eq)

data St = St
  { stScopes :: [Scope]
  -- ^ innermost first; empty means we are at the top level
  , stFun :: Maybe FunKind
  , stLoops :: Int
  , stClass :: ClassCtx
  , stErrors :: [ResolveError]
  }

type R = State St

resolveProgram :: Program Parsed -> Either [ResolveError] (Program Resolved)
resolveProgram (Program stmts) =
  case runState (mapM stmt stmts) (St [] Nothing 0 NoClass []) of
    (resolved, St{stErrors = []}) -> Right (Program resolved)
    (_, st) -> Left (reverse (stErrors st))

problem :: Pos -> Text -> R ()
problem p msg = modify' $ \s -> s{stErrors = ResolveError p msg : stErrors s}

-- Scopes ----------------------------------------------------------------------

-- | Run an action in a fresh scope, returning the result and the scope's size.
scoped :: R a -> R (a, Int)
scoped act = do
  modify' $ \s -> s{stScopes = Scope M.empty 0 : stScopes s}
  a <- act
  gets stScopes >>= \case
    Scope _ size : rest -> do
      modify' $ \s -> s{stScopes = rest}
      pure (a, size)
    [] -> error "scope stack underflow"

-- | Declare a name in the innermost scope, not yet usable.
declare :: Pos -> Name -> R Bind
declare p name =
  gets stScopes >>= \case
    [] -> pure BindGlobal
    Scope names size : rest -> do
      when (name `M.member` names) $
        problem p ("\"" <> name <> "\" on jo esitelty tässä lohkossa")
      let scope = Scope (M.insert name (size, False) names) (size + 1)
      modify' $ \s -> s{stScopes = scope : rest}
      pure (BindLocal size)

-- | Mark a declared name as initialized.
define :: Name -> R ()
define name = modify' $ \s -> case stScopes s of
  Scope names size : rest -> s{stScopes = Scope (M.adjust (fmap (const True)) name names) size : rest}
  [] -> s

declareDefined :: Pos -> Name -> R Bind
declareDefined p name = declare p name <* define name

lookupVar :: Pos -> Name -> R Loc
lookupVar p name = do
  scopes <- gets stScopes
  case findIndex (M.member name . scopeNames) scopes of
    Nothing -> pure Global
    Just hops -> do
      let (slot, ready) = scopeNames (scopes !! hops) M.! name
      unless ready $
        problem p ("muuttujaa \"" <> name <> "\" ei voi käyttää sen omassa alustuksessa")
      pure (Local hops slot)

-- | Temporarily change part of the state, restoring it afterwards.
with :: (St -> s) -> (s -> St -> St) -> s -> R a -> R a
with get set new act = do
  old <- gets get
  modify' (set new)
  a <- act
  modify' (set old)
  pure a

-- Statements ------------------------------------------------------------------

-- | Statements in a block that introduce a name directly into the block.
declaresSomething :: Stmt p -> Bool
declaresSomething = \case
  VarDecl{} -> True
  FunDecl{} -> True
  ClassDecl{} -> True
  _ -> False

stmt :: Stmt Parsed -> R (Stmt Resolved)
stmt = \case
  ExprStmt e -> ExprStmt <$> expr e
  VarDecl p name () initial -> do
    b <- declare p name
    initial' <- traverse expr initial
    define name
    pure (VarDecl p name b initial')
  FunDecl () f -> do
    b <- declareDefined (funPos f) (funName f)
    FunDecl b <$> function f
  ClassDecl p name () super methods -> do
    b <- declareDefined p name
    super' <- forM super $ \case
      Var sp sname () -> do
        when (sname == name) $ problem sp "luakka ei voi periä ittensä"
        Var sp sname <$> lookupVar sp sname
      _ -> error "parser only produces variables as superclasses"
    let ctx = maybe InClass (const InSubclass) super
        withSuper act = case super of
          Nothing -> act
          Just _ -> fst <$> scoped (declareDefined p "kanta" *> act)
    methods' <- with stClass (\c s -> s{stClass = c}) ctx . withSuper $
      fst <$> scoped (declareDefined p "itse" *> mapM function methods)
    pure (ClassDecl p name b super' methods')
  Print es -> Print <$> mapM expr es
  Block () stmts
    -- Blocks that declare nothing don't need a frame of their own at runtime.
    | any declaresSomething stmts -> uncurry (flip Block) <$> scoped (mapM stmt stmts)
    | otherwise -> Block 0 <$> mapM stmt stmts
  If c t e -> If <$> expr c <*> stmt t <*> traverse stmt e
  Loop c body step -> do
    c' <- expr c
    body' <- with stLoops (\n s -> s{stLoops = n}) 1 (stmt body)
    Loop c' body' <$> traverse expr step
  ForEach p name xs body -> do
    xs' <- expr xs
    (body', _) <- scoped $ do
      declareDefined p name
      with stLoops (\n s -> s{stLoops = n}) 1 (stmt body)
    pure (ForEach p name xs' body')
  Return p e -> do
    gets stFun >>= \case
      Nothing -> problem p "\"kylä lähtee\" toimii vaan roseduurin sisällä"
      Just InitializerKind | Just _ <- e -> problem p "alota-metodista ei voi palauttaa arvoa"
      _ -> pure ()
    Return p <$> traverse expr e
  Break p -> do
    loops <- gets stLoops
    when (loops == 0) $ problem p "\"ny riittää\" toimii vaan silmukassa"
    pure (Break p)
  Continue p -> do
    loops <- gets stLoops
    when (loops == 0) $ problem p "\"jatketaa\" toimii vaan silmukassa"
    pure (Continue p)
  Throw p e -> Throw p <$> expr e
  Try body p name handler -> do
    body' <- stmt body
    (handler', _) <- scoped (declareDefined p name *> stmt handler)
    pure (Try body' p name handler')

function :: Fun Parsed -> R (Fun Resolved)
function (Fun p name kind params body ()) =
  with stFun (\k s -> s{stFun = k}) (Just kind)
    -- Loops don't reach through function boundaries.
    . with stLoops (\n s -> s{stLoops = n}) 0
    $ do
      (body', size) <- scoped $ do
        zipWithM_ (\i param -> declareDefined p param >>= checkSlot i) [0 ..] params
        mapM stmt body
      pure (Fun p name kind params body' size)
 where
  -- Parameters must occupy the first slots, since that's where calls put the arguments.
  checkSlot :: Int -> Bind -> R ()
  checkSlot i b = unless (b == BindLocal i) $ problem p "sama parametri on kahesti"

-- Expressions -----------------------------------------------------------------

expr :: Expr Parsed -> R (Expr Resolved)
expr = \case
  Lit l -> pure (Lit l)
  Interp parts -> Interp <$> forM parts (\case Chunk t -> pure (Chunk t); Splice e -> Splice <$> expr e)
  Var p name () -> Var p name <$> lookupVar p name
  Assign p name () e -> do
    e' <- expr e
    loc <- lookupVar p name
    pure (Assign p name loc e')
  Unary p op e -> Unary p op <$> expr e
  Binary p op a b -> Binary p op <$> expr a <*> expr b
  Logical op a b -> Logical op <$> expr a <*> expr b
  Call p f args -> Call p <$> expr f <*> mapM expr args
  Lambda f -> Lambda <$> function f
  ListLit es -> ListLit <$> mapM expr es
  Index p a i -> Index p <$> expr a <*> expr i
  SetIndex p a i v -> SetIndex p <$> expr a <*> expr i <*> expr v
  Get p e name -> Get p <$> expr e <*> pure name
  SetProp p e name v -> SetProp p <$> expr e <*> pure name <*> expr v
  This p () -> do
    cls <- gets stClass
    when (cls == NoClass) $ problem p "\"itse\" toimii vaan luakan metodeissa"
    This p <$> lookupVar p "itse"
  Super p name () -> do
    gets stClass >>= \case
      NoClass -> problem p "\"kanta\" toimii vaan luakan metodeissa"
      InClass -> problem p "\"kanta\" toimii vaan luakassa jolla on kantaluakka"
      InSubclass -> pure ()
    Super p name <$> lookupVar p "kanta"
