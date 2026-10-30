-- | The Manse abstract syntax tree.
--
-- The tree is indexed by the compiler 'Phase' it is in ("trees that grow").
-- The parser produces @'Program' ''Parsed'@, where variables are plain names.
-- The resolver turns that into @'Program' ''Resolved'@, where every variable
-- reference knows exactly which stack frame and slot it lives in, and every
-- scope knows how many slots it needs. The interpreter only accepts resolved
-- programs, so "forgot to run the resolver" is a type error rather than a bug.
module Manse.Syntax
  ( Name
  , Source (..)
  , Pos (..)
  , Phase (..)
  , Loc (..)
  , Bind (..)
  , XVar
  , XBind
  , XFrame
  , Program (..)
  , Stmt (..)
  , Expr (..)
  , Fun (..)
  , FunKind (..)
  , Literal (..)
  , UnOp (..)
  , BinOp (..)
  , LogOp (..)
  , StrPart (..)
  , ForallX
  , binOpName
  ) where

import Data.Kind (Constraint, Type)
import Data.Text (Text)

type Name = Text

-- | A chunk of source code, kept around so errors can quote it.
data Source = Source {srcName :: FilePath, srcText :: Text}

-- | A position in the source: which file and the character offset into it.
-- Line and column are only computed when an error needs to be shown.
data Pos = Pos {posSource :: Source, posOffset :: Int}

instance Show Pos where
  show (Pos s o) = srcName s <> "@" <> show o

data Phase = Parsed | Resolved

-- | Where a resolved variable lives at runtime.
data Loc
  = -- | @Local hops slot@: walk @hops@ frames up the environment chain, then read @slot@.
    Local Int Int
  | -- | Not found in any enclosing scope; late-bound by name.
    Global
  deriving (Show, Eq)

-- | Where a declaration puts its value.
data Bind = BindLocal Int | BindGlobal
  deriving (Show, Eq)

-- | Extension point: what a variable /reference/ carries.
type family XVar (p :: Phase) :: Type where
  XVar Parsed = ()
  XVar Resolved = Loc

-- | Extension point: what a variable /declaration/ carries.
type family XBind (p :: Phase) :: Type where
  XBind Parsed = ()
  XBind Resolved = Bind

-- | Extension point: what a scope carries. Resolved scopes know their frame size.
type family XFrame (p :: Phase) :: Type where
  XFrame Parsed = ()
  XFrame Resolved = Int

-- | A constraint that holds for every extension field of a phase.
type ForallX (c :: Type -> Constraint) (p :: Phase) = (c (XVar p), c (XBind p), c (XFrame p))

newtype Program p = Program [Stmt p]

data Stmt (p :: Phase)
  = ExprStmt (Expr p)
  | VarDecl Pos Name (XBind p) (Maybe (Expr p))
  | FunDecl (XBind p) (Fun p)
  | ClassDecl Pos Name (XBind p) (Maybe (Expr p)) [Fun p]
  | Print [Expr p]
  | Block (XFrame p) [Stmt p]
  | If (Expr p) (Stmt p) (Maybe (Stmt p))
  | -- | @Loop cond body step@. @step@ runs after each iteration, also after @jatketaa@.
    Loop (Expr p) (Stmt p) (Maybe (Expr p))
  | ForEach Pos Name (Expr p) (Stmt p)
  | Return Pos (Maybe (Expr p))
  | Break Pos
  | Continue Pos
  | Throw Pos (Expr p)
  | Try (Stmt p) Pos Name (Stmt p)

data FunKind = FunctionKind | MethodKind | InitializerKind | LambdaKind
  deriving (Show, Eq)

data Fun (p :: Phase) = Fun
  { funPos :: Pos
  , funName :: Name
  , funKind :: FunKind
  , funParams :: [Name]
  , funBody :: [Stmt p]
  , funFrame :: XFrame p
  }

data Literal = LNum Double | LStr Text | LBool Bool | LNil
  deriving (Show, Eq)

data UnOp = Negate | Not
  deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Mod | Pow | Eq | Neq | Gt | Gte | Lt | Lte
  deriving (Show, Eq, Enum, Bounded)

data LogOp = And | Or
  deriving (Show, Eq)

data StrPart p = Chunk Text | Splice (Expr p)

data Expr (p :: Phase)
  = Lit Literal
  | Interp [StrPart p]
  | Var Pos Name (XVar p)
  | Assign Pos Name (XVar p) (Expr p)
  | Unary Pos UnOp (Expr p)
  | Binary Pos BinOp (Expr p) (Expr p)
  | Logical LogOp (Expr p) (Expr p)
  | Call Pos (Expr p) [Expr p]
  | Lambda (Fun p)
  | ListLit [Expr p]
  | Index Pos (Expr p) (Expr p)
  | SetIndex Pos (Expr p) (Expr p) (Expr p)
  | Get Pos (Expr p) Name
  | SetProp Pos (Expr p) Name (Expr p)
  | This Pos (XVar p)
  | -- | @kanta.method@. The 'XVar' locates @kanta@; @itse@ is always one frame closer.
    Super Pos Name (XVar p)

-- | How an operator is spelled in Manse, for error messages.
binOpName :: BinOp -> Text
binOpName = \case
  Add -> "plus"
  Sub -> "miinus"
  Mul -> "kertaa"
  Div -> "jaettuna"
  Mod -> "jakojäännös"
  Pow -> "potenssiin"
  Eq -> "on sama ku"
  Neq -> "ei oo sama ku"
  Gt -> "on isompi ku"
  Gte -> "on isompi tai sama ku"
  Lt -> "om piänempi ku"
  Lte -> "om piänempi tai sama ku"

deriving instance ForallX Show p => Show (Program p)
deriving instance ForallX Show p => Show (Stmt p)
deriving instance ForallX Show p => Show (Expr p)
deriving instance ForallX Show p => Show (Fun p)
deriving instance ForallX Show p => Show (StrPart p)
