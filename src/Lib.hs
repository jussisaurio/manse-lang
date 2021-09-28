{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Applicative (Alternative)
import Control.Monad (foldM, void, (>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import qualified Control.Monad.State as SM
import Data.Data
    ( Data(dataTypeOf, toConstr),
      Typeable,
      mkConstr,
      mkDataType,
      Constr,
      Fixity(Prefix) )
import Data.Functor ((<&>))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import Data.Void (Void)
import Text.Megaparsec
    ( optional,
      (<|>),
      anySingle,
      chunk,
      parse,
      satisfy,
      single,
      errorBundlePretty,
      many,
      manyTill,
      skipMany,
      some,
      Parsec,
      MonadParsec(try, notFollowedBy, eof),
      ParseError(TrivialError),
      ParseErrorBundle(bundleErrors) )
import Text.Megaparsec.Char
    ( alphaNumChar, char, eol, space, string )
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Stream (VisualStream)
import Debug.Trace
import Data.Bifunctor (first)
import Data.List.NonEmpty (toList)

class Constrable a where
  constr :: a -> Constr

instance {-# OVERLAPPABLE #-} Data a => Constrable a where
  constr = toConstr

instance {-# OVERLAPPING #-} Constrable a => Constrable (b -> a) where
  constr f = constr (f undefined)

(=|=) :: (Data a, Constrable b) => a -> b -> Bool
e =|= c = toConstr e == constr c

data ManseToken
  = TLeftParen
  | TRightParen
  | TLeftBrace
  | TRightBrace
  | TComma
  | TDot
  | TMinus
  | TPlus
  | TSemicolon
  | TSlash
  | TAsterisk
  | TBang
  | TBangEqual
  | TEqual
  | TDoubleEqual
  | TGreater
  | TGreaterEqual
  | TLess
  | TLessEqual
  | TIdentifier T.Text
  | TString T.Text
  | TNumber Double
  | TAnd
  | TClass
  | TElse
  | TFalse
  | TFun
  | TFor
  | TIf
  | TThen
  | TNil
  | TOr
  | TPrint
  | TReturn
  | TSuper
  | TThis
  | TTrue
  | TVariable
  | TWhile
  | EOF
  deriving (Eq, Show, Ord, Data, Typeable)

instance Constrable ManseToken where
  constr = toConstr

type Parser = Parsec Void T.Text

parseIdentifier :: Parser ManseToken
parseIdentifier = some (alphaNumChar <|> char '_') <&> TIdentifier . T.pack

parseStringLiteral :: Parser ManseToken
parseStringLiteral = char '"' >> manyTill Lexer.charLiteral (char '"') <&> TString . T.pack

parseNumberLiteral :: Parser ManseToken
parseNumberLiteral = (try Lexer.float <|> Lexer.decimal) <&> TNumber

parseKeyword :: T.Text -> Parser T.Text
parseKeyword = string >=> \kw -> notFollowedBy (alphaNumChar <|> char '_') >> pure kw

singleLineComment :: Parser ()
singleLineComment = string "//" >> many (satisfy (`notElem` ['\n', '\r'])) >> eol >> space

multiLineComment :: Parser ()
multiLineComment = string "/*" >> endOrAnythingElse >> space
 where
  endOrAnythingElse = string "*/" <|> (anySingle >> endOrAnythingElse)

parseToken :: Parser ManseToken
parseToken =
  space
    >> skipMany singleLineComment
    >> skipMany multiLineComment
    >> ( (TLeftParen <$ string "(")
          <|> (TRightParen <$ string ")")
          <|> (TLeftBrace <$ string "{")
          <|> (TRightBrace <$ string "}")
          <|> (TComma <$ string ",")
          <|> (TDot <$ string ".")
          <|> (TMinus <$ string "miinus ")
          <|> (TPlus <$ string "plus ")
          <|> (TSemicolon <$ string ";")
          <|> (TSlash <$ string "jaettuna ")
          <|> (TAsterisk <$ string "kertaa ")
          <|> try (TBangEqual <$ string "eijoo ")
          <|> (TBang <$ string "!")
          <|> try (TDoubleEqual <$ string "on sama ku ")
          <|> (TEqual <$ string "on ny ")
          <|> try (TGreaterEqual <$ string "on isompi tai sama ku ")
          <|> (TGreater <$ string "on isompi ku ")
          <|> try (TLessEqual <$ string "om piänempi tai sama ku ")
          <|> (TLess <$ string "om piänempi ku ")
          <|> try (TFun <$ parseKeyword "roseduuri")
          <|> try (TFor <$ parseKeyword "elikkä jos")
          <|> try (TIf <$ parseKeyword "jos")
          <|> try (TElse <$ parseKeyword "mut jos ei ni")
          <|> try (TThen <$ parseKeyword "ni")
          <|> try (TNil <$ parseKeyword "tyhjä")
          <|> try (TAnd <$ parseKeyword "ja")
          <|> try (TOr <$ parseKeyword "tai")
          <|> try (TPrint <$ parseKeyword "tulosta")
          <|> try (TReturn <$ parseKeyword "kylä lähtee")
          <|> try (TSuper <$ parseKeyword "kanta")
          <|> try (TThis <$ parseKeyword "itse")
          <|> try (TTrue <$ parseKeyword "tosi")
          <|> try (TFalse <$ parseKeyword "epätosi")
          <|> try (TVariable <$ parseKeyword "seonnääs nii et")
          <|> try (TWhile <$ parseKeyword "kuha")
          <|> parseNumberLiteral
          <|> parseStringLiteral
          <|> parseIdentifier
       )
      >>= \tok -> space >> pure tok

parseSourceFile :: Parser [ManseToken]
parseSourceFile = some parseToken

-- every operator is left associative except assignment
data Precedence = LAssoc Int | RAssoc Int deriving (Show, Eq, Data)

instance Ord Precedence where
  compare (LAssoc x) (LAssoc y)
    | x > y = GT
    | x == y = EQ
    | x < y = LT
    | otherwise = LT
  compare (LAssoc x) (RAssoc y)
    | x > y = GT
    | x == y = EQ
    | x < y = LT
    | otherwise = LT
  compare (RAssoc x) (LAssoc y)
    | x > y = GT
    | x == y = EQ
    | x < y = LT
    | otherwise = LT
  compare (RAssoc x) (RAssoc y)
    | x > y = GT
    | x == y = GT
    | x < y = LT
    | otherwise = LT

data ManseOperator = PrefixOp ManseToken Precedence (Exp -> Exp) | Binary ManseToken Precedence (Exp -> Exp -> Exp)

instance Show ManseOperator where
  show (PrefixOp t p f) = "PrefixOp " <> show t <> " " <> show p
  show (Binary t p f) = "Binary " <> show t <> " " <> show p

instance Data ManseOperator where
  toConstr (PrefixOp t p fn) = conPrefixOp
  toConstr (Binary t p fn) = conBinary
  dataTypeOf _ = tyMO

conPrefixOp = mkConstr tyMO "PrefixOp" [] Prefix
conBinary = mkConstr tyMO "Binary" [] Prefix
tyMO = mkDataType "Module.ManseOperator" [conPrefixOp, conBinary]

instance Constrable ManseOperator where
  constr = toConstr

data Exp
  = Negate Exp
  | Not Exp
  | Multiply Exp Exp
  | Divide Exp Exp
  | Add Exp Exp
  | Subtract Exp Exp
  | CompareGT Exp Exp
  | CompareGTE Exp Exp
  | CompareLT Exp Exp
  | CompareLTE Exp Exp
  | CompareEQ Exp Exp
  | CompareNEQ Exp Exp
  | And Exp Exp
  | Or Exp Exp
  | Ident T.Text
  | NumberLit Double
  | StringLit T.Text
  | BoolLit Bool
  | Parenthesized Exp
  | Assign Exp Exp
  | Call Exp [Exp]
  | Nil
  deriving (Eq, Show)

manseOperators :: [ManseOperator]
manseOperators =
  [ PrefixOp TMinus (LAssoc 70) Negate
  , PrefixOp TBang (LAssoc 70) Not
  , Binary TAsterisk (LAssoc 60) Multiply
  , Binary TSlash (LAssoc 60) Divide
  , Binary TPlus (LAssoc 50) Add
  , Binary TMinus (LAssoc 50) Subtract
  , Binary TGreater (LAssoc 40) CompareGT
  , Binary TGreaterEqual (LAssoc 40) CompareGTE
  , Binary TLess (LAssoc 40) CompareLT
  , Binary TLessEqual (LAssoc 40) CompareLTE
  , Binary TDoubleEqual (LAssoc 30) CompareEQ
  , Binary TBangEqual (LAssoc 30) CompareNEQ
  , Binary TAnd (LAssoc 20) And
  , Binary TOr (LAssoc 10) Or
  , Binary TEqual (RAssoc 5) Assign
  ]

mansePrefixOpOperators = filter (=|= PrefixOp) manseOperators

manseBinaryOperators = filter (=|= Binary) manseOperators

type ManseParser = Parsec Void [ManseToken]

parseExp :: Precedence -> ManseParser Exp
parseExp prec = do
  term <- parseTerm
  maybeCall <- optional (parseCall term prec)
  inf <- case maybeCall of
    Just c -> optional (parseInfix c prec)
    Nothing -> optional (parseInfix term prec)
  case inf of
    Just full -> pure full
    Nothing -> maybe (pure term) pure maybeCall

parseInfix :: Exp -> Precedence -> ManseParser Exp
parseInfix term requiredPrec = try $ do
  tok <- anySingle
  case L.find (\(Binary tok' _ _) -> tok == tok') manseBinaryOperators of
    Just (Binary tok'' prec op) -> do
      if tok'' == TEqual && not (isIdent term)
        then fail "Invalid left-hand side in assignment"
        else 
        if prec <= requiredPrec
            then fail "Precedence not high enough to continue"
            else do
              exp <- op term <$> parseExp prec
              more <- optional (parseInfix exp requiredPrec)
              case more of
                Just sth -> pure sth
                Nothing -> pure exp
     where
      isIdent = \case
        Ident _ -> True
        _ -> False
    _ -> fail "Not a binary operator"

parseTerm :: ManseParser Exp
parseTerm = try parsePrefixOp <|> try parsePrimary <|> parseParenthesized

parseCall :: Exp -> Precedence -> ManseParser Exp
parseCall term requiredPrec = do
  discard TLeftParen
  params <-
    optional
      ( do
          leading <-
            many $
              try
                ( do
                    e <- parseExp (LAssoc 0)
                    discard TComma
                    pure e
                )
          lastParam <- parseExp (LAssoc 0)
          pure (leading <> [lastParam])
      )
  discard TRightParen
  pure $ Call term $ fromMaybe [] params

parsePrefixOp :: ManseParser Exp
parsePrefixOp = do
  tok <- anySingle
  case L.find (\(PrefixOp tok' _ _) -> tok == tok') mansePrefixOpOperators of
    Just (PrefixOp tok'' prec op) -> op <$> parseExp prec
    _ -> fail "Not a unary operator"

parsePrimary :: ManseParser Exp
parsePrimary =
  try $
    anySingle >>= \case
      TString str -> pure $ StringLit str
      TNumber num -> pure $ NumberLit num
      TNil -> pure Nil
      TIdentifier ident -> pure $ Ident ident
      TTrue -> pure $ BoolLit True
      TFalse -> pure $ BoolLit False
      _ -> fail "foo"

parseParenthesized :: ManseParser Exp
parseParenthesized = do
  discard TLeftParen
  exp <- parseExp (LAssoc 0)
  discard TRightParen
  pure $ Parenthesized exp

data Declaration = Declaration T.Text (Maybe Exp) deriving (Eq, Show)
data FunctionDeclaration = FunctionDeclaration T.Text [T.Text] BlockStatement deriving (Eq, Show)
newtype ExpStatement = ExpStatement Exp deriving (Eq, Show)
data IfStatement = IfStatement Exp BlockStatement (Maybe BlockStatement) deriving (Eq, Show)
data ForStatement = ForStatement (Either Declaration ExpStatement) (Maybe Exp) (Maybe Exp) BlockStatement deriving (Eq, Show)
data WhileStatement = WhileStatement Exp BlockStatement deriving (Eq, Show)
newtype ReturnStatement = ReturnStatement (Maybe Exp) deriving (Eq, Show)
newtype PrintStatement = PrintStatement Exp deriving (Eq, Show)
newtype BlockStatement = BlockStatement [Statement] deriving (Eq, Show)

data Statement
  = D Declaration
  | E ExpStatement
  | If IfStatement
  | For ForStatement
  | While WhileStatement
  | Return ReturnStatement
  | Print PrintStatement
  | Block BlockStatement
  | Func FunctionDeclaration
  deriving (Eq, Show)

newtype Program = Program [Statement] deriving (Eq, Show)

-- STATEMENTS

parseProgram :: ManseParser Program
parseProgram = do
  stmts <- many parseStatement
  void eof
  pure $ Program stmts

parseStatement =
  try (parseDeclaration <&> D)
    <|> try (parseExpStmt <&> E)
    <|> try (parseIfStatement <&> If)
    <|> try (parseForStatement <&> For)
    <|> try (parseWhileStatement <&> While)
    <|> try (parseReturnStatement <&> Return)
    <|> try (parsePrintStatement <&> Print)
    <|> try (parseFunctionDeclaration <&> Func)
    <|> (parseBlockStatement <&> Block)

class Discardable a where
  discard :: a -> ManseParser ()

instance Discardable ManseToken where
  discard = void . single

instance Discardable [ManseToken] where
  discard = void . chunk

parseFunctionDeclaration :: ManseParser FunctionDeclaration
parseFunctionDeclaration = do
  discard TFun
  TIdentifier fnName <- satisfy (=|= TIdentifier)
  discard TLeftParen
  params <-
    optional
      ( do
          leading <-
            many $
              try
                ( do
                    pn <- satisfy (=|= TIdentifier)
                    discard TComma
                    pure pn
                )
          lastParam <- satisfy (=|= TIdentifier)
          pure (leading <> [lastParam])
      )
  discard TRightParen
  block <- parseBlockStatement
  pure $ case params of
    Just p -> FunctionDeclaration fnName (map (\(TIdentifier i) -> i) p) block
    Nothing -> FunctionDeclaration fnName [] block

parseDeclaration :: ManseParser Declaration
parseDeclaration =
  do
    discard TVariable
    TIdentifier v <- satisfy (=|= TIdentifier)
    let ident = v
     in try
          ( do
              discard TSemicolon
              pure $ Declaration ident Nothing
          )
          <|> do
            discard TEqual
            exp <- parseExp (LAssoc 0)
            discard TSemicolon
            pure $ Declaration ident (Just exp)

parseExpStmt :: ManseParser ExpStatement
parseExpStmt = do
  e <- parseExp (LAssoc 0)
  discard TSemicolon
  pure (ExpStatement e)

parseBlockStatement :: ManseParser BlockStatement
parseBlockStatement = do
  discard TLeftBrace
  stmts <- many parseStatement
  discard TRightBrace
  pure $ BlockStatement stmts

parseIfStatement :: ManseParser IfStatement
parseIfStatement = do
  discard [TIf, TLeftParen]
  e <- parseExp (LAssoc 0)
  discard [TRightParen, TThen]
  trueBlock <- parseBlockStatement
  maybeFalseBlock <- optional $ single TElse >> parseBlockStatement
  pure (IfStatement e trueBlock maybeFalseBlock)

parseForStatement :: ManseParser ForStatement
parseForStatement = do
  discard [TFor, TLeftParen]
  firstStm <- try (Left <$> parseDeclaration) <|> (Right <$> parseExpStmt)
  secondStm <- optional $ parseExp (LAssoc 0)
  thirdStm <- optional $ case secondStm of
    (Just _) -> single TSemicolon >> parseExp (LAssoc 0)
    Nothing -> parseExp (LAssoc 0)
  discard [TRightParen, TThen]
  ForStatement firstStm secondStm thirdStm <$> parseBlockStatement

parseWhileStatement :: ManseParser WhileStatement
parseWhileStatement = do
  discard [TWhile, TLeftParen]
  cond <- parseExp (LAssoc 0)
  discard [TRightParen, TThen]
  WhileStatement cond <$> parseBlockStatement

parseReturnStatement :: ManseParser ReturnStatement
parseReturnStatement = do
  discard TReturn
  retExp <- optional $ parseExp (LAssoc 0)
  discard TSemicolon
  pure $ ReturnStatement retExp

parsePrintStatement :: ManseParser PrintStatement
parsePrintStatement = do
  discard TPrint
  e <- parseExp (LAssoc 0)
  discard TSemicolon
  pure $ PrintStatement e

parseTextToProgram txt = 
  case first errorBundlePretty $ parse parseSourceFile "" txt of
    Left err -> Left err
    Right ts -> case parse parseProgram "" ts of
                      Left peb -> Left $ show peb
                      Right pro -> Right pro

parseFile :: String -> IO (Either () Program)
parseFile fn = do
  f <- T.pack <$> readFile fn
  case parse parseSourceFile fn f of
    Right tokens -> case parse parseProgram "tokens" tokens of
      Right program -> pure $ Right program
      Left errbundle -> do
        mapM_
          ( \case
              TrivialError offset (Just unexpected) expected -> do
                print ("Error at offset " ++ show offset)
                print ("Unexpected token: " ++ show unexpected)
                print ("Expected one of: " ++ show expected)
              _ -> print "Something unexpected happened"
          )
          $ bundleErrors errbundle
        pure $ Left ()
    Left errbundle -> do
      print $ errorBundlePretty errbundle
      pure (Left ())

data RuntimeValue = VNumber Double | VString T.Text | VBoolean Bool | VNil | VFunc (FunctionDeclaration, Environment) deriving (Eq)

instance Show RuntimeValue where
  show (VNumber n) = let isInt x = x == fromInteger (round x) in if isInt n then show (round n) else show n
  show (VString s) = show s
  show (VBoolean b) = show b
  show VNil = "nil"
  show (VFunc (FunctionDeclaration n _ _, _)) = show ("<roseduuri " <> n <> ">")

data Environment = Environment {parent :: Maybe Environment, variables :: M.Map T.Text RuntimeValue} deriving (Eq, Show)

newtype RuntimeError = RuntimeError String deriving (Show, Eq)

runtimeError :: String -> ExecutionContext a
runtimeError = throwError . RuntimeError

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

type ExecutionContext a = SM.StateT Environment (ExceptT RuntimeError Identity) a
execute m env = runIdentity . runExceptT $ SM.evalStateT m env
executeAndReturnState m env = runIdentity . runExceptT $ SM.runStateT m env
tryExecute m env = either throwError pure $ execute m env
tryExecuteAndReturnState m env = either throwError pure $ executeAndReturnState m env

interpretStatement :: Statement -> ExecutionContext (Maybe RuntimeValue)
interpretStatement stmt = case stmt of
  D (Declaration varN maybeExpr) -> do
    env <- SM.get
    case M.lookup varN (variables env) of
      Just v -> runtimeError "Redeclaration of variable"
      Nothing -> case maybeExpr of
        Nothing -> SM.modify (\env' -> env'{variables = M.insert varN VNil (variables env')}) >> pure Nothing
        Just exp -> do
          val <- evalExp exp
          SM.modify (\env' -> env'{variables = M.insert varN val (variables env')}) >> pure Nothing
  E (ExpStatement e) -> evalExp e >> pure Nothing
  If (IfStatement condExp trueB maybeFalseB) -> do
    condition <- isTruthy <$> evalExp condExp
    if condition
      then interpretStatement (Block trueB)
      else case maybeFalseB of
        Nothing -> pure Nothing
        Just falseB -> interpretStatement (Block falseB)
  For (ForStatement init maybeExp1 maybeExp2 (BlockStatement stmts)) -> do
    void $ case init of
      Left de -> interpretStatement $ D de
      Right e -> interpretStatement $ E e
    -- desugar to while loop
    case (maybeExp1, maybeExp2) of
      (Just e1, Just e2) -> interpretStatement $ While $ WhileStatement e1 (BlockStatement (stmts <> [E $ ExpStatement e2]))
      (Just e1, Nothing) -> interpretStatement $ While $ WhileStatement e1 (BlockStatement stmts)
      (Nothing, Just e2) -> interpretStatement $ While $ WhileStatement e2 (BlockStatement stmts)
      (Nothing, Nothing) -> interpretStatement $ While $ WhileStatement (BoolLit True) (BlockStatement stmts)
  While (WhileStatement a block) ->
    let loop = evalExp a >>= (\truthy -> if not truthy then pure Nothing else do
          maybeReturned <- interpretStatement (Block block)
          maybe loop (pure . Just) maybeReturned) . isTruthy
      in loop

  Return (ReturnStatement e) -> case e of
    Nothing -> pure $ Just VNil
    Just e -> evalExp e <&> Just
  Print (PrintStatement p) -> pure Nothing -- todo handle prints
  Block (BlockStatement stmts) -> do
    env <- SM.get
    runAsChildScopeOf env $ shortCircuitReturn stmts
  Func (FunctionDeclaration name params block) -> do
    env <- SM.get
    case M.lookup name (variables env) of
      Just v -> runtimeError "Redeclaration of function"
      Nothing -> do
        SM.modify (\env' -> 
          let newVariables = M.insert name (VFunc (FunctionDeclaration name params block, newEnv)) (variables env')
              newEnv = env'{variables = newVariables}
              in newEnv
          )
        pure Nothing

shortCircuitReturn :: [Statement] -> ExecutionContext (Maybe RuntimeValue)
shortCircuitReturn = foldM ( \m s -> if isJust m then pure m else interpretStatement s) Nothing

callFn :: FunctionDeclaration -> [RuntimeValue] -> ExecutionContext RuntimeValue
callFn (FunctionDeclaration n params block) args =
  if length args /= length params
    then runtimeError ("wrong num of args: expected " <> show params <> ", got arg count: " <> show (length args))
    else do
      SM.modify (\env -> let newVars = foldr (\(k, v) m -> M.insert k v m) (variables env) (zip params args) in env{variables = newVars})
      -- not sure why I need this tbh, figure out later #somethingrotteninthestateofdenmark
      SM.modify (\env -> let newVars = M.insert n (VFunc (FunctionDeclaration n params block, env)) (variables env) in env{variables = newVars})
      a <- let (BlockStatement stmts) = block in shortCircuitReturn stmts
      pure $ fromMaybe VNil a

runAsChildScopeOf :: Environment -> ExecutionContext a -> ExecutionContext a
runAsChildScopeOf par m = do
  (v, newState) <- tryExecuteAndReturnState m $ Environment{parent = Just par, variables = M.empty}
  SM.modify(\env' -> fromMaybe env' (parent newState))
  pure v

interpretProgram :: Program -> [RuntimeValue] -> ExecutionContext RuntimeValue
interpretProgram (Program stmts) args = do
  case safeHead stmts of
    Just stmt -> do
      interpretStatement stmt
      interpretProgram (Program $ tail stmts) args
    Nothing -> do
      main <- SM.gets (M.lookup "pää" . variables)
      case main of
        Just (VFunc (decl, closure)) -> runAsChildScopeOf closure (callFn decl args)
        _ -> runtimeError "pää is not a function"

isTruthy :: RuntimeValue -> Bool
isTruthy VNil = False
isTruthy (VBoolean False) = False
isTruthy _ = True

recursiveLookup :: T.Text -> ExecutionContext (Maybe RuntimeValue)
recursiveLookup str = do
  env <- SM.get
  case (M.lookup str (variables env), parent env) of
    (Just v, _) -> pure $ Just v
    (Nothing, Nothing) -> pure Nothing
    (Nothing, Just upperCtx) -> tryExecute (recursiveLookup str) upperCtx

recursiveAssign str v = do
  env <- SM.get
  case (M.lookup str (variables env), parent env) of
    (Just val, _) -> SM.put env{variables = M.insert str v (variables env)} >> pure v
    (Nothing, Nothing) -> runtimeError ("Assigning to undefined variable " <> show str)
    (Nothing, Just upperCtx) -> do
        (_, st) <- tryExecuteAndReturnState (recursiveAssign str v) upperCtx
        SM.modify(\env' -> env' { parent = Just st })
        pure v
  

evalExp :: Exp -> ExecutionContext RuntimeValue
evalExp exp = case exp of
  Not e -> evalExp e <&> VBoolean . isTruthy
  Negate e -> do
    result <- evalExp e
    case result of
      (VNumber n) -> pure $ VNumber (-1 * n)
      _ -> runtimeError "Cannot negate a value that is not a number"
  Multiply e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VNumber $ a * b)
      _ -> runtimeError "Cannot multiply a non-number"
  Divide e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VNumber $ a / b)
      _ -> runtimeError "Cannot divide a non-number"
  Add e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VNumber $ a + b)
      _ -> runtimeError "Cannot add a non-number"
  Subtract e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VNumber $ a - b)
      _ -> runtimeError "Cannot subtract a non-number"
  CompareGT e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a > b)
      _ -> runtimeError "Cannot compare > a non-number"
  CompareGTE e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a >= b)
      _ -> runtimeError "Cannot compare >= a non-number"
  CompareLT e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a < b)
      _ -> runtimeError "Cannot compare < a non-number"
  CompareLTE e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a <= b)
      _ -> runtimeError "Cannot compare <= a non-number"
  CompareEQ e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a == b)
      (VString a, VString b) -> pure (VBoolean $ a == b)
      (VBoolean a, VBoolean b) -> pure (VBoolean $ a == b)
      (VNil, VNil) -> pure (VBoolean True)
      _ -> pure $ VBoolean False
  CompareNEQ e1 e2 -> do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
      (VNumber a, VNumber b) -> pure (VBoolean $ a /= b)
      (VString a, VString b) -> pure (VBoolean $ a /= b)
      (VBoolean a, VBoolean b) -> pure (VBoolean $ a /= b)
      (VNil, VNil) -> pure (VBoolean False)
      _ -> runtimeError "Cannot test for equality unless both are same type and one of: number, string, boolean or nil"
  And e1 e2 -> do
    r1 <- evalExp e1
    if isTruthy r1
      then do
        r2 <- evalExp e2
        pure $ VBoolean (isTruthy r2)
      else pure $ VBoolean False
  Or e1 e2 -> do
    r1 <- evalExp e1
    if not $ isTruthy r1
      then do
        r2 <- evalExp e2
        pure $ VBoolean (isTruthy r2)
      else pure $ VBoolean True
  Ident str -> do
    maybeValue <- recursiveLookup str
    case maybeValue of
      Nothing -> runtimeError ("Undefined variable" <> show str)
      Just val -> pure val
  NumberLit n -> pure $ VNumber n
  StringLit str -> pure $ VString str
  BoolLit bool -> pure $ VBoolean bool
  Parenthesized e -> evalExp e
  Assign e1 e2 -> do
    case e1 of
      Ident str -> do 
          r <- evalExp e2
          recursiveAssign str r
      _ -> runtimeError ("Invalid left-hand side in assignment:" <> show e1)
  Nil -> pure VNil
  Call exp args ->
    case exp of
      (Ident fnName) -> do
        env <- SM.get
        maybeFn <- recursiveLookup fnName
        case maybeFn of
          Nothing -> runtimeError ("trying to call undefined function " <> show fnName)
          Just (VFunc (decl, closure)) -> do
            vals <- mapM evalExp args
            runAsChildScopeOf closure (callFn decl vals)
          _ -> runtimeError (show fnName <> " is not callable")
      _ -> runtimeError "Expression is not callable"

runProgramWithArgs program args = execute (interpretProgram program args) $ Environment{parent = Nothing, variables = M.empty}

runProgram sourceFile args = do
  p <- parseFile sourceFile
  case p of
    Right program -> case execute (interpretProgram program []) $ Environment{parent = Nothing, variables = M.empty} of
      Right retval -> print retval
      Left a -> print a
    Left a -> print "error lol"