module Typechecker

import Grammar

Theta : Type
Theta = List FunDecl

Renames : Type
Renames = (Nat, List (String, Nat))

lookupRename : String -> Renames -> Either Error Nat
lookupRename name renames = case renames of
  (_, []) => Left (SyntaxError $ "undefined variable " ++ name)
  (g, (n, idx) :: rest) => 
    if n == name then 
        Right idx 
      else 
        lookupRename name (g, rest)

extendRenames : String -> Renames -> Renames
extendRenames name (g, rest) = (S g, (name, g) :: rest)

lookupGamma : Nat -> FunGamma -> FunType
lookupGamma n gamma = case (n, gamma) of
    (_, EmptyGamma) => lookupGamma n gamma -- this should be impossible
    (0, AddGamma _ t _) => t
    (S n, AddGamma _ _ gamma) => lookupGamma n gamma

lookupTheta : String -> Theta -> Either Error FunDecl
lookupTheta name theta = case theta of
  [] => Left (SyntaxError $ "undefined declaration " ++ name)
  (decl :: rest) => case decl of
    DataDecl n _ => if n == name then Right decl else lookupTheta name rest
    CodataDecl n _ => if n == name then Right decl else lookupTheta name rest
    FuncDecl n _ _ _ => if n == name then Right decl else lookupTheta name rest

eq : FunType -> FunType -> Bool
eq t u = case (t, u) of
  (IntType, IntType) => True
  (UserType n, UserType m) => n == m
  _ => False

check_args_types : Renames -> Theta -> FunGamma -> List TermSyntax -> FunGamma -> Either Error FunArguments
infer_args_types : Renames -> Theta -> FunGamma -> List TermSyntax -> Either Error (FunArguments, FunGamma)

check_prod_type : Renames -> Theta -> FunGamma -> TermSyntax -> FunType -> Either Error FunProducer
infer_prod_type : Renames -> Theta -> FunGamma -> TermSyntax -> Either Error (FunProducer, FunType)

check_cons_type : Renames -> Theta -> FunGamma -> TermSyntax -> FunType -> Either Error FunConsumer
infer_cons_type : Renames -> Theta -> FunGamma -> TermSyntax -> Either Error (FunConsumer, FunType)

check_args_types renames theta gamma args gamma' = case (args, gamma') of
  ([], EmptyGamma) => Right NoArgs
  (arg :: rest, AddGamma _ argt gamma2) => do
    arg2 <- check_prod_type renames theta gamma arg argt
    rest2 <- check_args_types renames theta gamma rest gamma2
    Right $ ProdArgs arg2 rest2
  _ => Left $ TypeError "bad number of arguments"

infer_args_types renames theta gamma args = case args of
  [] => Right (NoArgs, EmptyGamma)
  (arg :: rest) => do
    (arg2, argt) <- infer_prod_type renames theta gamma arg
    (rest2, gamma2) <- infer_args_types renames theta gamma rest
    Right (ProdArgs arg2 rest2, AddGamma Positive argt gamma2)

infer_prod_type renames theta gamma arg = case arg of
  IdentSyntax s => do
    idx <- lookupRename s renames
    Right (IdentTerm idx, lookupGamma idx gamma)
  IntLitSyntax n => Right (IntTerm n, IntType)
  LetSyntax s (Just t) v scope => do
    v2 <- check_prod_type renames theta gamma v t
    (scope2, scopet) <- infer_prod_type (extendRenames s renames) theta (AddGamma Positive t gamma) scope
    Right (LetTerm v2 scope2, scopet)
  LetSyntax s Nothing v scope => do
    (v2, vtype) <- infer_prod_type renames theta gamma v
    (scope2, scopet) <- infer_prod_type (extendRenames s renames) theta (AddGamma Positive vtype gamma) scope
    Right (LetTerm v2 scope2, scopet)
  CaseSyntax scrutinee branches => do
    (scrut2, scrutt) <- infer_prod_type renames theta gamma scrutinee
    case scrutt of
      IntType => Left (TypeError "Case not implemented")
      UserType s => case lookupTheta s theta of
        Right (DataDecl _ []) => Left $ TypeError "Can't infer type of empty case statement"
        Right (DataDecl _ ((c, g) :: xs)) => 
          ? 
          -- lookup the `c` branch, error if it's not found
          -- infer the type it returns
          -- go through the branches and check that they all return that type
        Right (CodataDecl _ _) => Left $ TypeError "Can't scrutinize codata"
        Right (FuncDecl _ _ _ _) => Left $ TypeError "Can't scrutinize function"
        Left err => Left err
  ObjectSyntax methods => Left (TypeError "Object not implemented")
  MethodAppSyntax ob name args => Left (TypeError "MethodApp not implemented")
  GlobalAppSyntax name args => Left (TypeError "GlobalApp not implemented")
  ConstructorAppSyntax name args => Left (TypeError "ConstructorApp not implemented")
