-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Parser

import Grammar

record Parser a where
  constructor MkParser
  parse : List Char -> (Either Error (a, List Char))

public export
parse : Parser a -> String -> Either Error a
parse p str = case p.parse (unpack str) of
  Left err => Left err
  Right (x, xs) => Right x

satisfy : (Char -> Bool) -> Parser Char
satisfy p = MkParser
  (\str => case str of
    [] => Left (SyntaxError "Unexpected end of input")
    (x :: xs) => if p x then Right (x, xs) else Left (SyntaxError $ "unexpected " ++ pack [x]))

-- Always fail with a message
err : String -> Parser a
err msg = MkParser (\_ => Left (SyntaxError msg))

-- Look at next character without consuming
notP : Parser a -> Parser Unit
notP p = MkParser (\str => case p.parse str of
  Left err => Right ((), str)
  Right (x, xs) => 
    case str of
      [] => Left (SyntaxError "EOF")
      (x :: xs) => Left (SyntaxError $ "unexpected " ++ pack [x]))

-- Parse an exact character
char : Char -> Parser Char
char c = satisfy (== c)

-- Delay constructing a parser until parse-time (to break recursive references)
lazyP : (Unit -> Parser a) -> Parser a
lazyP f = MkParser (\str => (f ()).parse str)

Functor Parser where
  map f (MkParser p) = MkParser (\str => case p str of
    Left err => Left err
    Right (x, xs) => Right (f x, xs))

Applicative Parser where
  pure x = MkParser (\str => Right (x, str))
  (MkParser p) <*> (MkParser q) = MkParser (\str => case p str of
    Left err => Left err
    Right (f, xs) => case q xs of
      Left err => Left err
      Right (x, ys) => Right (f x, ys))

Monad Parser where
  (>>=) p f = MkParser (\str => case p.parse str of
    Left err => Left err
    Right (x, xs) => case (f x).parse xs of
      Left err => Left err
      Right (y, ys) => Right (y, ys))

sequence_ : List (Parser a) -> Parser ()
sequence_ [] = pure ()
sequence_ (p :: ps) = do _ <- p; sequence_ ps

-- Parse an exact string
stringP : String -> Parser String
stringP s = do
  _ <- sequence_ (map char (unpack s))
  pure s

-- One-or-more
many1 : Parser a -> Parser (a, List a)
many1 p = MkParser (\str => case p.parse str of
  Left err => Left err
  Right (x, xs) => case (many1 p).parse xs of
    Left err => Right ((x, []), xs)
    Right ((y, ys), zs) => Right ((x, y :: ys), zs))

-- Zero-or-more
many0 : Parser a -> Parser (List a)
many0 p = MkParser (\str => case (many1 p).parse str of
  Left err => Right ([], str)
  Right ((x, xs), ys) => Right (x :: xs, ys))

-- either
(<|>) : Parser a -> Parser a -> Parser a
(<|>) p q = MkParser (\str => case p.parse str of
  Left _ => q.parse str
  Right (x, xs) => Right (x, xs))

-- zero-or-more whitespace
ws0 : Parser ()
ws0 = do _ <- many0 (satisfy isSpace); pure ()

-- one-or-more whitespace
ws1 : Parser ()
ws1 = MkParser (\str => case (many1 (satisfy isSpace)).parse str of
  Left err => Left err
  Right (_, rest) => Right ((), rest))

perhaps : Parser a -> Parser (Maybe a)
perhaps p = MkParser (\str => case p.parse str of
  Left _ => Right (Nothing, str)
  Right (x, xs) => Right (Just x, xs))

eof : Parser ()
eof = MkParser (\str => case str of
  [] => Right ((), [])
  (x :: xs) => Left (SyntaxError ("unexpected " ++ pack [x])))

identChar : Char -> Bool
identChar c = isAlphaNum c || c == '_'

parseIdentName : Parser String
parseIdentName = do
  first <- satisfy isLower
  rest <- many0 (satisfy identChar)
  pure $ pack (first :: rest)

keyword : String -> Parser String
keyword w = do
  _ <- stringP w
  _ <- notP (satisfy identChar)
  pure w

parens : Parser a -> Parser a
parens p = do
  _ <- char '('
  x <- p
  _ <- char ')'
  pure x

parseIntLit : Parser Grammar.TermSyntax
parseIntLit = do
  first <- satisfy isDigit
  digits <- many0 (satisfy isDigit)
  pure (Grammar.IntLitSyntax (cast (pack (first :: digits))))

reserved : List String
reserved = ["let", "in", "Int", "data", "fn"]

parseIdent : Parser Grammar.TermSyntax
parseIdent = do
  name <- parseIdentName
  if elem name reserved
    then err ("Reserved word: " ++ name)
    else pure (Grammar.IdentSyntax name)

-- Forward declarations (needed for recursive grammar)
parseExpr : Parser Grammar.TermSyntax
parseType : Parser Grammar.FunType

parseCtorApp : Parser Grammar.TermSyntax
parseCtorApp = do
  first <- satisfy isUpper
  rest <- many0 (satisfy identChar)
  let name = pack (first :: rest)
  args <- many0 (lazyP (\_ => parseExpr))
  pure $ ConstructorAppSyntax name args

parseAtom : Parser Grammar.TermSyntax
parseAtom =
  parens (lazyP (\_ => parseExpr))
  <|> parseCtorApp
  <|> parseIntLit
  <|> parseIdent

-- Let binding: let x : T = a in b
parseLet : Parser Grammar.TermSyntax
parseLet = do
  _ <- keyword "let"
  _ <- ws0
  name <- parseIdentName
  _ <- ws0
  t <- perhaps (do _ <- char ':'; parseType)
  _ <- ws0
  _ <- char '='
  val <- lazyP (\_ => parseExpr)
  _ <- keyword "in"
  body <- lazyP (\_ => parseExpr)
  pure (Grammar.LetSyntax name t val body)

-- Expression: let has lowest precedence, then atoms
parseExpr = do
  _ <- ws0
  x <- parseLet <|> parseAtom
  _ <- ws0
  pure x

parseIntType : Parser Grammar.FunType
parseIntType = do
  _ <- keyword "Int"
  pure Grammar.IntType

parseUserType : Parser Grammar.FunType
parseUserType = do
  first <- satisfy isUpper
  rest <- many0 $ satisfy identChar
  let name = pack $ first :: rest
  if elem name reserved
    then err ("Reserved word: " ++ name)
    else pure (Grammar.UserType name)

parseType = do
  _ <- ws0
  t <- parseIntType <|> parseUserType
  _ <- ws0
  pure t

-- Parse a parameter: name : Type or _name : Type
parseParam : Parser (String, Grammar.FunType)
parseParam = do
  _ <- char '('
  _ <- ws0
  name <- parseIdentName
  _ <- ws0
  _ <- char ':'
  t <- parseType
  _ <- char ')'
  pure (name, t)

-- Parse constructor: Name or Name Type1 Type2 ...
parseConstructor : Parser (String, Grammar.GammaSyntax)
parseConstructor = do
  first <- satisfy isUpper
  rest <- many0 $ satisfy identChar
  let name = pack $ first :: rest
  if elem name reserved then err name else pure ()
  _ <- ws0
  -- Parse zero or more types
  types <- many0 (do
    t <- parseType
    _ <- ws0
    pure ("", t))
  pure (name, types)

-- Parse data declaration: data Name = Cons1 | Cons2 | ...
parseDataDecl : Parser Grammar.DeclSyntax
parseDataDecl = do
  _ <- keyword "data"
  _ <- ws1
  first <- satisfy isUpper
  rest <- many0 (satisfy identChar)
  let name = pack $ first :: rest
  _ <- ws0
  _ <- char '='
  _ <- ws0
  -- Parse first constructor
  first <- parseConstructor
  -- Parse remaining constructors (each preceded by |)
  rest <- many0 (do
    _ <- char '|'
    _ <- ws0
    parseConstructor)
  pure (Grammar.DataDeclSyntax name (first :: rest))

-- Parse function declaration: fn name (params) : RetType = body
parseFuncDecl : Parser Grammar.DeclSyntax
parseFuncDecl = do
  _ <- keyword "fn"
  _ <- ws1
  name <- parseIdentName
  -- Parse parameters in parentheses
  params  <- many0 $ do
    _ <- ws0
    p <- parseParam
    pure p
  _ <- ws0
  _ <- char ':'
  retType <- parseType
  _ <- char '='
  body <- parseExpr
  pure (Grammar.FuncDeclSyntax name params retType body)

-- Parse a single declaration
parseDecl : Parser Grammar.DeclSyntax
parseDecl = parseDataDecl <|> parseFuncDecl

public export
parseTop : Parser (List Grammar.DeclSyntax)
parseTop = do
  _ <- ws0
  -- Parse zero or more declarations
  decls <- many0 (do
    d <- parseDecl
    _ <- ws0
    pure d)
  _ <- eof
  pure decls
