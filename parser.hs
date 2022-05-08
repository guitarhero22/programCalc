
-- Expressions are function compositions
newtype Expr = Compose [Atom] deriving Eq

-- Atoms are simply variables or function applications
data Atom = Var VarName | Con ConName [Expr]
            deriving Eq
type VarName = String -- Variable Names
type ConName = String -- ?Function Names

newtype Parser a = Parser [(a, String)]

-- expr :: Parser Expr
-- expr 

simple :: Parser Expr
simple = 