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

parse : Parser a -> String -> Either Error a
parse p str = case p.parse (unpack str) of
  Left err => Left err
  Right (x, xs) => Right x

satisfy : (Char -> Bool) -> Parser Char
satisfy p = MkParser
  (\str => case str of
    [] => Left (Unexpected "Unexpected end of input")
    (x :: xs) => if p x then Right (x, xs) else Left (Unexpected $ pack [x]))

-- Always fail with a message
err : String -> Parser a
err msg = MkParser (\_ => Left (Unexpected msg))

-- Look at next character without consuming
notP : Parser a -> Parser Unit
notP p = MkParser (\str => case p.parse str of
  Left err => Right ((), str)
  Right (x, xs) => 
    case str of
      [] => Left (Unexpected "EOF")
      (x :: xs) => Left (Unexpected $ pack [x]))

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
  (x :: xs) => Left (Unexpected (pack [x])))

identChar : Char -> Bool
identChar c = isAlphaNum c || c == '_'

parseIdentName : Parser String
parseIdentName = do
  first <- satisfy isAlpha
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

parseIntLit : Parser Grammar.Syntax
parseIntLit = do
  first <- satisfy isDigit
  digits <- many0 (satisfy isDigit)
  pure (Grammar.IntLitSyntax (cast (pack (first :: digits))))

reserved : List String
reserved = ["let", "in"]

parseIdent : Parser Grammar.Syntax
parseIdent = do
  name <- parseIdentName
  if elem name reserved
    then err ("Reserved word: " ++ name)
    else pure (Grammar.IdentSyntax name)

-- Forward declarations (needed for recursive grammar)
parseExpr : Parser Grammar.Syntax
parseType : Parser Grammar.TypeSyntax

parseAtom : Parser Grammar.Syntax
parseAtom =
  parens (lazyP (\_ => parseExpr))
  <|> parseIntLit
  <|> parseIdent

parseParamNoAnnot : Parser (String, Maybe Grammar.TypeSyntax)
parseParamNoAnnot = do
  name <- parseIdentName
  pure (name, Nothing)

parseParamAnnot : Parser (String, Maybe Grammar.TypeSyntax)
parseParamAnnot = do
  _ <- char '('
  name <- parseIdentName
  _ <- char ':'
  _ <- ws0
  ty <- parseType
  _ <- char ')'
  pure (name, Just ty)

-- Lambda: \x y z. expr (desugared to nested lambdas)
parseLambda : Parser Grammar.Syntax
parseLambda = do
  _ <- char '\\'
  (arg0, rest) <- many1 (ws0 >> (parseParamNoAnnot <|> parseParamAnnot))
  let args = arg0 :: rest
  _ <- ws0
  _ <- char '.'
  _ <- ws0
  body <- lazyP (\_ => parseExpr)
  let build : List (String, Maybe Grammar.TypeSyntax) -> Grammar.Syntax -> Grammar.Syntax
      build [] acc = acc
      build ((x, t) :: as) acc = Grammar.LambdaSyntax x t (build as acc)
  pure (build args body)

-- Let binding: let x = a in b
parseLet : Parser Grammar.Syntax
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

-- Application: left-associative, allows juxtaposition only when next token is '('
parseApp : Parser Grammar.Syntax
parseApp = do
  f <- parseAtom
  args <- many0 (ws1 >> parseAtom)
  pure (foldl Grammar.AppSyntax f args)

-- Expression: let/lambda have lowest precedence; application higher; atoms highest
parseExpr = do
  _ <- ws0
  x <- parseLet <|> parseLambda <|> parseApp
  _ <- ws0
  pure x

parseIntType : Parser Grammar.TypeSyntax
parseIntType = do 
  _ <- keyword "Int"
  pure Grammar.IntTypeSyntax

parseTypeAtom : Parser Grammar.TypeSyntax
parseTypeAtom = parens (lazyP (\_ => parseType)) <|> parseIntType

parseType = do
  _ <- ws0
  t <- parseTypeAtom
  rest <- many0 $ do
    _ <- ws0
    _ <- stringP "->"
    _ <- ws0
    parseTypeAtom
  pure (foldr Grammar.FuncTypeSyntax t rest)


parseTop : Parser Grammar.Syntax
parseTop = do
  e <- parseExpr
  _ <- eof
  pure e
