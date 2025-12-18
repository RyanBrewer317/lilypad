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

check_case_branches : Renames -> Theta -> FunGamma -> List (String, GammaSyntax, TermSyntax) -> FunType -> Either Error (List (String, FunArguments, FunProducer))
infer_first_branch : Renames -> Theta -> FunGamma -> String -> List (String, GammaSyntax, TermSyntax) -> FunGamma -> Either Error FunType

lookupConstructor : String -> Theta -> Either Error (FunGamma, FunType)
lookupConstructor cname [] = Left $ TypeError ("constructor not found: " ++ cname)
lookupConstructor cname (decl :: rest) = case decl of
  DataDecl typeName constructors => case findInConstructors cname constructors of
    Just gamma => Right (gamma, UserType typeName)
    Nothing => lookupConstructor cname rest
  _ => lookupConstructor cname rest
  where
    findInConstructors : String -> List (String, FunGamma) -> Maybe FunGamma
    findInConstructors _ [] = Nothing
    findInConstructors name ((n, g) :: rest) =
      if n == name then Just g else findInConstructors name rest

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

-- Find the branch for the given constructor name and infer its return type
infer_first_branch renames theta gamma cname branches cGamma = case branches of
  [] => Left $ TypeError ("missing case branch for constructor " ++ cname)
  ((name, vars, body) :: rest) =>
    if name == cname then do
      let (renames', gamma') = extendBranchVars renames gamma vars cGamma
      (_, retType) <- infer_prod_type renames' theta gamma' body
      Right retType
    else
      infer_first_branch renames theta gamma cname rest cGamma
  where
    extendBranchVars : Renames -> FunGamma -> GammaSyntax -> FunGamma -> (Renames, FunGamma)
    extendBranchVars rens gam [] EmptyGamma = (rens, gam)
    extendBranchVars rens gam ((varName, _) :: restVars) (AddGamma pol typ restGamma) =
      let rens' = extendRenames varName rens
          gam' = AddGamma pol typ gam
          (rens'', gam'') = extendBranchVars rens' gam' restVars restGamma
      in (rens'', gam'')
    extendBranchVars rens gam _ _ = (rens, gam)

-- Check that all branches return the expected type
check_case_branches renames theta gamma branches expectedType = case branches of
  [] => Right []
  ((name, vars, body) :: rest) => do
    branchResult <- case lookupConstructor name theta of
      Right (cGamma, _) => do
        let (renames', gamma') = extendBranchVars renames gamma vars cGamma
        body2 <- check_prod_type renames' theta gamma' body expectedType
        Right (name, varsToArgs (length vars) vars cGamma, body2)
      Left err => Left err
    rest2 <- check_case_branches renames theta gamma rest expectedType
    Right (branchResult :: rest2)
  where
    extendBranchVars : Renames -> FunGamma -> GammaSyntax -> FunGamma -> (Renames, FunGamma)
    extendBranchVars rens gam [] EmptyGamma = (rens, gam)
    extendBranchVars rens gam ((varName, _) :: restVars) (AddGamma pol typ restGamma) =
      let rens' = extendRenames varName rens
          gam' = AddGamma pol typ gam
          (rens'', gam'') = extendBranchVars rens' gam' restVars restGamma
      in (rens'', gam'')
    extendBranchVars rens gam _ _ = (rens, gam)

    -- Convert pattern variables to FunArguments, with indices 0..n-1 where n is the number of vars
    varsToArgs : Nat -> GammaSyntax -> FunGamma -> FunArguments
    varsToArgs _ [] EmptyGamma = NoArgs
    varsToArgs n (_ :: restVars) (AddGamma Positive _ restGamma) =
      ProdArgs (IdentTerm (minus n 1)) (varsToArgs (minus n 1) restVars restGamma)
    varsToArgs n (_ :: restVars) (AddGamma Negative _ restGamma) =
      ConsArgs (ContTerm (minus n 1)) (varsToArgs (minus n 1) restVars restGamma)
    varsToArgs _ _ _ = NoArgs

convertGammaSyntax : GammaSyntax -> FunGamma
convertGammaSyntax [] = EmptyGamma
convertGammaSyntax ((_, t) :: rest) = AddGamma Positive t (convertGammaSyntax rest)

checkDecl : Theta -> DeclSyntax -> Either Error FunDecl
checkDecl theta decl = case decl of
  DataDeclSyntax name constructors =>
    Right $ DataDecl name (map (\(cname, gamma) => (cname, convertGammaSyntax gamma)) constructors)
  CodataDeclSyntax _ _ => Left $ TypeError "Codata declarations not yet supported"
  FuncDeclSyntax name gamma retType body => do
    let gamma' = convertGammaSyntax gamma
    let renames = (length gamma, reverse $ buildRenames 0 gamma)
    body' <- check_prod_type renames theta gamma' body retType
    Right $ FuncDecl name gamma' retType body'
  where
    buildRenames : Nat -> GammaSyntax -> List (String, Nat)
    buildRenames _ [] = []
    buildRenames n ((varName, _) :: rest) = (varName, n) :: buildRenames (S n) rest

export
checkProgram : List DeclSyntax -> Either Error (List FunDecl)
checkProgram decls = checkProgramHelper [] decls
  where
    checkProgramHelper : Theta -> List DeclSyntax -> Either Error (List FunDecl)
    checkProgramHelper theta [] = Right []
    checkProgramHelper theta (decl :: rest) = do
      funDecl <- checkDecl theta decl
      restDecls <- checkProgramHelper (funDecl :: theta) rest
      Right (funDecl :: restDecls)

check_prod_type renames theta gamma arg expectedType = do
  (term, inferredType) <- infer_prod_type renames theta gamma arg
  if eq expectedType inferredType then
      Right term
    else
      Left $ TypeError ("Type mismatch: expected " ++ show expectedType ++ ", got " ++ show inferredType)
  where
    show : FunType -> String
    show IntType = "Int"
    show (UserType n) = n

check_cons_type renames theta gamma arg expectedType = case arg of
  IdentSyntax s => do
    idx <- lookupRename s renames
    let inferredType = lookupGamma idx gamma
    if eq expectedType inferredType then
        Right (ContTerm idx)
      else
        Left $ TypeError ("Type mismatch in continuation: expected " ++ show expectedType ++ ", got " ++ show inferredType)
  _ => Left $ TypeError "Only variables can be used as continuations"
  where
    show : FunType -> String
    show IntType = "Int"
    show (UserType n) = n

infer_cons_type renames theta gamma arg = case arg of
  IdentSyntax s => do
    idx <- lookupRename s renames
    Right (ContTerm idx, lookupGamma idx gamma)
  _ => Left $ TypeError "Only variables can be used as continuations"

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
        Right (DataDecl _ ((c, g) :: xs)) => do
            -- lookup the `c` branch, error if it's not found
            -- infer the type it returns
            retType <- infer_first_branch renames theta gamma c branches g
            -- go through the branches and check that they all return that type
            branches2 <- check_case_branches renames theta gamma branches retType
            Right (CaseTerm scrut2 branches2, retType)
        Right (CodataDecl _ _) => Left $ TypeError "Can't scrutinize codata"
        Right (FuncDecl _ _ _ _) => Left $ TypeError "Can't scrutinize function"
        Left err => Left err
  ObjectSyntax methods => Left (TypeError "Object not implemented")
  MethodAppSyntax ob name args => Left (TypeError "MethodApp not implemented")
  GlobalAppSyntax name args => Left (TypeError "GlobalApp not implemented")
  ConstructorAppSyntax name args => do
    (cGamma, retType) <- lookupConstructor name theta
    args2 <- check_args_types renames theta gamma args cGamma
    Right (ConstructorAppTerm name args2, retType)
