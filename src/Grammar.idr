-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Grammar

public export
data Syntax : Type where
  IntLitSyntax : Int -> Syntax
  IdentSyntax : String -> Syntax
  LambdaSyntax : String -> Syntax -> Syntax
  AppSyntax : Syntax -> Syntax -> Syntax
  LetSyntax : String -> Syntax -> Syntax -> Syntax
