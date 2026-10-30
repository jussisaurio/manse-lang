-- | Parser for Manse source code, straight from 'Text' to a @'Program' ''Parsed'@.
module Manse.Parser
  ( parseProgram
  , parseReplInput
  , ReplInput (..)
  , ParseError
  , renderParseError
  , reservedWords
  ) where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Char (isAlphaNum, isLetter)
import Data.Functor (($>))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Manse.Syntax
import Text.Megaparsec hiding (ParseError, Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = ParsecT Void Text (Reader Source)

type ParseError = ParseErrorBundle Text Void

-- | Parse a whole source file.
parseProgram :: Source -> Either ParseError (Program Parsed)
parseProgram src = runParse src (sc *> (Program <$> many statement) <* eof)

data ReplInput = ReplExpr (Expr Parsed) | ReplStmts (Program Parsed)

-- | In the REPL, a bare expression without a semicolon gets its value printed.
parseReplInput :: Source -> Either ParseError ReplInput
parseReplInput src = runParse src (sc *> p <* eof)
 where
  p = try (ReplExpr <$> expr <* optional semi <* eof) <|> (ReplStmts . Program <$> many statement)

runParse :: Source -> Parser a -> Either ParseError a
runParse src p = runReader (runParserT p (srcName src) (srcText src)) src

renderParseError :: ParseError -> String
renderParseError e =
  "Voi pyhä Sylvi, tästä ei saa mittään selvää!\n\n" <> T.unpack (finnish (T.pack (errorBundlePretty e)))
 where
  -- megaparsec only speaks English, so translate the bits it adds itself.
  finnish = T.unlines . map line . T.lines
  line l
    | Just rest <- T.stripPrefix "unexpected " l = "en oottanu tätä: " <> common rest
    | Just rest <- T.stripPrefix "expecting " l = "oottasin: " <> common rest
    | otherwise = l
  common =
    T.replace " or " " tai "
      . T.replace "end of input" "tiedoston loppu"
      . T.replace "end of line" "rivin loppu"

-- Lexical structure -----------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

pos :: Parser Pos
pos = Pos <$> asks id <*> getOffset

isIdentStart, isIdentChar :: Char -> Bool
isIdentStart c = isLetter c || c == '_'
isIdentChar c = isAlphaNum c || c == '_'

-- | Words that can never be used as names.
reservedWords :: Set.Set Text
reservedWords =
  Set.fromList
    [ "seonnääs", "roseduuri", "jos", "ni", "mut", "kuha", "elikkä", "kylä", "tyhjä"
    , "ja", "tai", "tulosta", "kanta", "itse", "tosi", "epätosi", "luakka", "niinku"
    , "yritä", "paiskaa", "jokaselle", "tästä", "ny", "jatketaa", "plus", "miinus"
    , "kertaa", "jaettuna", "jakojäännös", "potenssiin", "on", "om", "ei", "eijoo"
    , "sitte", "eli"
    ]

-- | A keyword, possibly several words long (@"kylä lähtee"@). Each word must end
-- at a word boundary, and any amount of whitespace may separate the words.
keyword :: Text -> Parser ()
keyword kw = label (T.unpack ("\"" <> kw <> "\"")) . try $ mapM_ word (T.words kw)
 where
  word w = lexeme (string w *> notFollowedBy (satisfy isIdentChar))

identifier :: Parser Name
identifier = label "nimi" . try . lexeme $ do
  o <- getOffset
  name <- T.cons <$> satisfy isIdentStart <*> takeWhileP Nothing isIdentChar
  when (name `Set.member` reservedWords) $ do
    setOffset o
    fail ("\"" <> T.unpack name <> "\" on varattu sana, sitä ei voi käyttää nimenä")
  pure name

semi :: Parser ()
semi = label "puolipiste" (symbol ";")

parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepEndBy` symbol ","

number :: Parser Double
number = label "luku" . lexeme $ try L.float <|> (fromInteger <$> L.decimal)

-- | A string literal, possibly with @{interpolated}@ expressions.
stringLit :: Parser (Expr Parsed)
stringLit = label "teksti" . lexeme $ do
  char '"'
  parts <- manyTill part (char '"')
  pure $ case mergeChunks parts of
    [] -> Lit (LStr "")
    [Chunk t] -> Lit (LStr t)
    ps -> Interp ps
 where
  part =
    (Splice <$> between (char '{' *> sc) (char '}') expr)
      <|> (Chunk . T.singleton <$> (char '\\' *> escape))
      <|> (Chunk <$> takeWhile1P Nothing (`notElem` ['"', '\\', '{']))
  escape =
    choice
      [ char 'n' $> '\n'
      , char 't' $> '\t'
      , char 'r' $> '\r'
      , char '0' $> '\0'
      , char '"'
      , char '\\'
      , char '{'
      , char '}'
      ]
  mergeChunks = \case
    Chunk a : Chunk b : rest -> mergeChunks (Chunk (a <> b) : rest)
    p : rest -> p : mergeChunks rest
    [] -> []

-- Statements ------------------------------------------------------------------

statement :: Parser (Stmt Parsed)
statement =
  label "lause" $
    choice
      [ varDecl
      , funDecl
      , classDecl
      , printStmt
      , ifStmt
      , whileStmt
      , forStmt
      , forEachStmt
      , returnStmt
      , Break <$> (pos <* keyword "ny riittää") <* semi
      , Continue <$> (pos <* keyword "jatketaa") <* semi
      , throwStmt
      , tryStmt
      , block
      , ExprStmt <$> expr <* semi
      ]

varDecl :: Parser (Stmt Parsed)
varDecl = do
  keyword "seonnääs nii et"
  p <- pos
  name <- identifier
  initial <- optional (keyword "on ny" *> expr)
  semi
  pure (VarDecl p name () initial)

funDecl :: Parser (Stmt Parsed)
funDecl = do
  p <- pos
  -- Only commit once we know it's a named function and not a lambda.
  try (keyword "roseduuri" *> lookAhead identifier)
  name <- identifier
  FunDecl () <$> funRest p name FunctionKind

-- | Parameter list and body. The body is either a block, or @eli expr@ as a
-- shorthand for a block that returns @expr@. The shorthand ends in a semicolon,
-- except in lambdas, which are expressions.
funRest :: Pos -> Name -> FunKind -> Parser (Fun Parsed)
funRest p name kind = do
  params <- parens (commaSep identifier)
  body <- blockBody <|> shortBody
  pure (Fun p name kind params body ())
 where
  shortBody = do
    keyword "eli"
    rp <- pos
    e <- expr
    when (kind /= LambdaKind) semi
    pure [Return rp (Just e)]

classDecl :: Parser (Stmt Parsed)
classDecl = do
  keyword "luakka"
  p <- pos
  name <- identifier
  super <- optional (keyword "niinku" *> (Var <$> pos <*> identifier <*> pure ()))
  methods <- braces (many method)
  pure (ClassDecl p name () super methods)
 where
  method = do
    optional (keyword "roseduuri")
    p <- pos
    name <- identifier
    funRest p name (if name == "alota" then InitializerKind else MethodKind)

printStmt :: Parser (Stmt Parsed)
printStmt = keyword "tulosta" *> (Print <$> (expr `sepBy` symbol ",")) <* semi

ifStmt :: Parser (Stmt Parsed)
ifStmt = keyword "jos" *> ifRest
 where
  ifRest = do
    cond <- expr
    keyword "ni"
    thenB <- block
    elseB <- optional (keyword "mut" *> keyword "jos" *> (elseBlock <|> ifRest))
    pure (If cond thenB elseB)
  elseBlock = keyword "ei ni" *> block

whileStmt :: Parser (Stmt Parsed)
whileStmt = do
  keyword "kuha"
  cond <- expr
  keyword "ni"
  body <- block
  pure (Loop cond body Nothing)

-- | @elikkä jos (init; cond; step) ni { ... }@, desugared into a block holding
-- the initializer and a loop.
forStmt :: Parser (Stmt Parsed)
forStmt = do
  keyword "elikkä jos"
  (initial, cond, step) <- parens $ do
    initial <- (semi $> Nothing) <|> (Just <$> (varDecl <|> (ExprStmt <$> expr <* semi)))
    cond <- optional expr <* semi
    step <- optional expr
    pure (initial, cond, step)
  keyword "ni"
  body <- block
  let loop = Loop (maybe (Lit (LBool True)) id cond) body step
  pure (Block () (maybe [loop] (\i -> [i, loop]) initial))

forEachStmt :: Parser (Stmt Parsed)
forEachStmt = do
  keyword "jokaselle"
  p <- pos
  name <- identifier
  keyword "tästä"
  xs <- expr
  keyword "ni"
  ForEach p name xs <$> block

returnStmt :: Parser (Stmt Parsed)
returnStmt = do
  p <- pos
  keyword "kylä lähtee"
  Return p <$> optional expr <* semi

throwStmt :: Parser (Stmt Parsed)
throwStmt = do
  p <- pos
  keyword "paiskaa"
  Throw p <$> expr <* semi

-- | @yritä { ... } jos käy kalpaten (virhe) ni { ... }@
tryStmt :: Parser (Stmt Parsed)
tryStmt = do
  keyword "yritä"
  body <- block
  keyword "jos käy kalpaten"
  p <- pos
  name <- parens identifier
  keyword "ni"
  Try body p name <$> block

blockBody :: Parser [Stmt Parsed]
blockBody = braces (many statement)

block :: Parser (Stmt Parsed)
block = Block () <$> blockBody

-- Expressions -----------------------------------------------------------------

-- | Assignment is the loosest-binding expression, and right associative.
expr :: Parser (Expr Parsed)
expr = label "lauseke" $ do
  lhs <- pipeline
  optional (hidden (keyword "on ny") *> ((,) <$> pos <*> expr)) >>= \case
    Nothing -> pure lhs
    Just (p, rhs) -> case lhs of
      Var vp name () -> pure (Assign vp name () rhs)
      Index ip obj ix -> pure (SetIndex ip obj ix rhs)
      Get gp obj name -> pure (SetProp gp obj name rhs)
      _ -> do
        setOffset (posOffset p)
        fail "tähän ei voi sijoittaa mittään"

-- | @x sitte f(y)@ means @f(x, y)@, and @x sitte f@ means @f(x)@.
pipeline :: Parser (Expr Parsed)
pipeline = do
  first <- operators
  rest <- many ((,) <$> (pos <* hidden (keyword "sitte")) <*> operators)
  pure (foldl' pipe first rest)
 where
  pipe acc (p, stage) = case stage of
    Call cp f args -> Call cp f (acc : args)
    f -> Call p f [acc]

operators :: Parser (Expr Parsed)
operators = makeExprParser postfixExpr table
 where
  table =
    [ [prefix "miinus" (`Unary` Negate), prefix "ei" (`Unary` Not), prefix "!" (`Unary` Not)]
    , [infixR "potenssiin" Pow]
    , [infixL "kertaa" Mul, infixL "jaettuna" Div, infixL "jakojäännös" Mod]
    , [infixL "plus" Add, infixL "miinus" Sub]
    , [ infixL "on isompi tai sama ku" Gte
      , infixL "on isompi ku" Gt
      , infixL "om piänempi tai sama ku" Lte
      , infixL "on piänempi tai sama ku" Lte
      , infixL "om piänempi ku" Lt
      , infixL "on piänempi ku" Lt
      ]
    , [infixL "on sama ku" Eq, infixL "ei oo sama ku" Neq, infixL "eijoo" Neq]
    , [InfixL (op "ja" $> Logical And)]
    , [InfixL (op "tai" $> Logical Or)]
    ]
  -- Operators are hidden from "expecting ..." lists in errors; there are just too many.
  op kw = hidden (if kw == "!" then symbol kw else keyword kw)
  prefix kw f = Prefix (foldr1 (.) <$> some (f <$> (pos <* op kw)))
  infixL kw o = InfixL ((\p -> Binary p o) <$> (pos <* op kw))
  infixR kw o = InfixR ((\p -> Binary p o) <$> (pos <* op kw))

-- | A primary expression followed by any number of calls, indexings and
-- property accesses: @f(1)(2)[3].x@.
postfixExpr :: Parser (Expr Parsed)
postfixExpr = do
  e <- primary
  suffixes <- many suffix
  pure (foldl' (flip ($)) e suffixes)
 where
  suffix =
    choice
      [ (\p args e -> Call p e args) <$> pos <*> parens (commaSep expr)
      , (\p ix e -> Index p e ix) <$> pos <*> brackets expr
      , (\p name e -> Get p e name) <$> (symbol "." *> pos) <*> identifier
      ]

primary :: Parser (Expr Parsed)
primary =
  choice
    [ Lit . LNum <$> number
    , stringLit
    , keyword "tosi" $> Lit (LBool True)
    , keyword "epätosi" $> Lit (LBool False)
    , keyword "tyhjä" $> Lit LNil
    , This <$> (pos <* keyword "itse") <*> pure ()
    , superExpr
    , lambda
    , ListLit <$> brackets (commaSep expr)
    , parens expr
    , Var <$> pos <*> identifier <*> pure ()
    ]
 where
  superExpr = do
    p <- pos
    keyword "kanta"
    symbol "."
    name <- identifier
    pure (Super p name ())
  lambda = do
    p <- pos
    keyword "roseduuri"
    Lambda <$> funRest p "nimetön" LambdaKind
