-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Parser

import Grammar

data Error : Type where
  Unexpected : String -> Error

record Parser a where
  constructor MkParser
  parse : List Char -> (Either Error (a, List Char))

-- Run a parser on a String and return either an error or the parsed value.
parse : Parser a -> String -> Either Error a
parse p str = case p.parse (unpack str) of
  Left err => Left err
  Right (x, xs) => Right x

-- Basic primitive
satisfy : (Char -> Bool) -> Parser Char
satisfy p = MkParser
  (\str => case str of
    [] => Left (Unexpected "Unexpected end of input")
    (x :: xs) => if p x then Right (x, xs) else Left (Unexpected $ pack [x]))

-- Always fail with a message
err : String -> Parser a
err msg = MkParser (\_ => Left (Unexpected msg))

-- Look at next character without consuming
peek : Parser (Maybe Char)
peek = MkParser (\str => case str of
  [] => Right (Nothing, str)
  (x :: xs) => Right (Just x, str))

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

-- Parse an exact string
sequence_ : List (Parser a) -> Parser ()
sequence_ [] = pure ()
sequence_ (p :: ps) = do _ <- p; sequence_ ps

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

-- Alternative
(<|>) : Parser a -> Parser a -> Parser a
(<|>) p q = MkParser (\str => case p.parse str of
  Left _ => q.parse str
  Right (x, xs) => Right (x, xs))

-- Helpers for whitespace and tokens

-- zero-or-more whitespace
ws0 : Parser ()
ws0 = do _ <- many0 (satisfy isSpace); pure ()

-- one-or-more whitespace
ws1 : Parser ()
ws1 = MkParser (\str => case (many1 (satisfy isSpace)).parse str of
  Left err => Left err
  Right (_, rest) => Right ((), rest))

-- Consume leading and trailing whitespace around a parser
lexeme : Parser a -> Parser a
lexeme p = do
  _ <- ws0
  x <- p
  pure x

-- Parse a symbol (no word boundary rules)
symbol : String -> Parser String
symbol s = lexeme (stringP s)

-- End-of-input
eof : Parser ()
eof = MkParser (\str => case str of
  [] => Right ((), [])
  (x :: xs) => Left (Unexpected (pack [x])))

-- Identifiers and keywords

identChar : Char -> Bool
identChar c = isAlphaNum c || c == '_'

-- Identifier name (String), excluding reserved words
parseIdentName : Parser String
parseIdentName = lexeme $ do
  first <- satisfy isAlpha
  rest <- many0 (satisfy identChar)
  let s = pack (first :: rest)
  if s == "let" || s == "in"
     then err "Unexpected reserved word"
     else pure s

-- Reserved keyword with a word boundary: e.g., "let", "in"
keyword : String -> Parser String
keyword w = do
  _ <- ws0
  _ <- stringP w
  -- ensure next char is not an ident char (word boundary)
  la <- peek
  let checkBoundary : Parser ()
      checkBoundary = case la of
        Just c => if identChar c then err "Expected word boundary after keyword" else pure ()
        Nothing => pure ()
  _ <- checkBoundary
  _ <- ws0
  pure w

-- Parentheses
parens : Parser a -> Parser a
parens p = do
  _ <- symbol "("
  x <- p
  _ <- symbol ")"
  pure x

-- Literals and atomic terms

parseIntLit : Parser Grammar.Syntax
parseIntLit = lexeme $ do
  first <- satisfy isDigit
  digits <- many0 (satisfy isDigit)
  pure (Grammar.IntLitSyntax (cast (pack (first :: digits))))

parseIdent : Parser Grammar.Syntax
parseIdent = map Grammar.IdentSyntax parseIdentName

-- Forward declaration (needed for recursive grammar)
parseExpr : Parser Grammar.Syntax

parseAtom : Parser Grammar.Syntax
parseAtom =
  parens (lazyP (\_ => parseExpr))
  <|> parseIntLit
  <|> parseIdent

-- Lambda: \x y z -> expr (desugared to nested lambdas)
parseLambda : Parser Grammar.Syntax
parseLambda = do
  _ <- lexeme (char '\\')
  args1 <- many1 parseIdentName
  let collect : (String, List String) -> List String
      collect (x, xs) = x :: xs
  let args = collect args1
  _ <- symbol "->"
  body <- lazyP (\_ => parseExpr)
  let build : List String -> Grammar.Syntax -> Grammar.Syntax
      build [] acc = acc
      build (a :: as) acc = Grammar.LambdaSyntax a (build as acc)
  pure (build args body)

-- Let binding: let x = a in b
parseLet : Parser Grammar.Syntax
parseLet = do
  _ <- keyword "let"
  name <- parseIdentName
  _ <- symbol "="
  val <- lazyP (\_ => parseExpr)
  _ <- keyword "in"
  body <- lazyP (\_ => parseExpr)
  pure (Grammar.LetSyntax name val body)

-- Application: left-associative, allows juxtaposition only when next token is '('
parseApp : Parser Grammar.Syntax
parseApp = do
  f <- parseAtom
  args <- many0 ((do _ <- ws1; parseAtom) <|> parens (lazyP (\_ => parseExpr)))
  pure (foldl Grammar.AppSyntax f args)

-- Expression: let/lambda have lowest precedence; application higher; atoms highest
parseExpr = parseLet <|> parseLambda <|> parseApp

-- Optional: a top-level program parser that requires complete consumption
parseTop : Parser Grammar.Syntax
parseTop = do
  _ <- ws0
  e <- parseExpr
  _ <- ws0
  _ <- eof
  pure e
