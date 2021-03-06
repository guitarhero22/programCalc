module Expressions (
    Expr (Compose), 
    Atom (Var, Con),
    VarName,
    ConName,
    expr,
    deCompose,
)
where
import Utilities
import Parsing
import Data.Char
import Data.List

-- Expressions are function compositions
newtype Expr = Compose [Atom] deriving Eq

-- Atoms are simply variables or function applications
data Atom = Var VarName | Con ConName [Expr]
            deriving Eq
type VarName = String -- Variable Names
type ConName = String -- ?Function Names

expr :: Parser Expr
expr = simple >>= rest
    where
        rest :: Expr -> Parser Expr
        rest s1 = (do 
            op <- operator
            s2 <- simple
            return (Compose [Con op [s1, s2]])
            ) <|> return s1

simple :: Parser Expr
simple = do 
        es <- somewith (symbol ".") term;
        return (Compose (concatMap deCompose es))

term :: Parser Expr
term = ident args <|> paren expr
args = many (ident Parsing.none <|> paren expr)

ident :: Parser [Expr] -> Parser Expr
ident args = do
            x <- token (some (sat isAlphaNum))
            Parsing.guard (isAlpha (head x))
            if isVar x
                then return (Compose [Var x])
            else if x == "id"
                then return (Compose [])
            else
                do 
                    as <- args
                    return (Compose [Con x as])

isVar :: String -> Bool
isVar [x] = True
isVar [x, d] = isDigit d
isVar _ = False

deCompose :: Expr -> [Atom]
deCompose (Compose as) = as

-- operator
operator :: Parser String
operator = do 
            op <- token (some (sat symbolic))
            Parsing.guard (op /= "." && op /= "=")
            return op

symbolic = (`elem` opsymbols)
opsymbols = "!@#$%&*+./<=>?\\^|:-~"

showSep :: String -> (a -> ShowS) -> [a] -> ShowS
showSep sep f
    = compose . intersperse (showString sep) . map f

showSpace = showChar ' '

isOp = all symbolic 

instance Show Expr where
    showsPrec p (Compose []) = showString "id"
    showsPrec p (Compose [a]) = showsPrec p a
    showsPrec p (Compose as) = 
            showParen (p>0) (showSep " . " (showsPrec 1) as)

instance Show Atom where
    showsPrec p (Var v) = showString v
    showsPrec p (Con f []) = showString f
    showsPrec p (Con f [e1, e2]) 
        | isOp f = showParen (p>0) (showsPrec 1 e1 . showSpace . showString f . showSpace . showsPrec 1 e2)

    showsPrec p (Con f es)
        = showParen (p > 1) (showString f . showSpace . showSep " " (showsPrec 2) es)