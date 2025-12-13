-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Grammar

public export
data Positivity = Positive | Negative

public export
data FunType : Type where
  IntType : FunType
  UserType : String -> FunType

public export
GammaSyntax : Type
GammaSyntax = List (String, FunType)

public export
data TermSyntax : Type where
  IntLitSyntax : Int -> TermSyntax
  IdentSyntax : String -> TermSyntax
  GlobalAppSyntax : String -> List TermSyntax -> TermSyntax
  ConstructorAppSyntax : String -> List TermSyntax -> TermSyntax
  CaseSyntax : TermSyntax -> List (String, GammaSyntax, TermSyntax) -> TermSyntax
  ObjectSyntax : List (String, GammaSyntax, TermSyntax) -> TermSyntax
  MethodAppSyntax : TermSyntax -> String -> List TermSyntax -> TermSyntax
  LetSyntax : String -> Maybe FunType -> TermSyntax -> TermSyntax -> TermSyntax

public export
data DeclSyntax : Type where
  DataDeclSyntax : String -> List (String, GammaSyntax) -> DeclSyntax
  CodataDeclSyntax : String -> List (String, GammaSyntax, FunType) -> DeclSyntax
  FuncDeclSyntax : String -> GammaSyntax -> FunType -> TermSyntax -> DeclSyntax

mutual
  public export
  data FunProducer : Type where
    IntTerm : Int -> FunProducer
    IdentTerm : Nat -> FunProducer
    LetTerm : FunProducer -> FunProducer -> FunProducer
    GlobalAppTerm : String -> FunArguments -> FunProducer
    ConstructorAppTerm : String -> FunArguments -> FunProducer
    CaseTerm : FunProducer -> List (String, FunArguments, FunProducer) -> FunProducer
    ObjectTerm : List (String, FunGamma, FunProducer) -> FunProducer
    MethodAppTerm : FunProducer -> String -> FunArguments -> FunProducer
  public export
  data FunConsumer : Type where
    ContTerm : Nat -> FunConsumer
  public export
  data FunArguments : Type where
    NoArgs : FunArguments
    ProdArgs : FunProducer -> FunArguments -> FunArguments
    ConsArgs : FunConsumer -> FunArguments -> FunArguments
  public export
  data FunGamma : Type where
    EmptyGamma : FunGamma
    AddGamma : Positivity -> FunType -> FunGamma -> FunGamma
  public export
  data FunDecl : Type where
    DataDecl : String -> List (String, FunGamma) -> FunDecl
    CodataDecl : String -> List (String, FunGamma, FunType) -> FunDecl
    FuncDecl : String -> FunGamma -> FunType -> FunProducer -> FunDecl

public export
data Error : Type where
  TypeError : String -> Error
  SyntaxError : String -> Error
