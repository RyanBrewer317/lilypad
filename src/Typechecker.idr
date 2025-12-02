module Typechecker

import Grammar

data Def : Type where
  EmptyDef : Def
  AddDef : String -> Syntax -> Def -> Def

-- check : global_defs -> gamma -> expr -> type -> either error or unit
-- infer : global_defs -> gamma -> expr -> either error or type
