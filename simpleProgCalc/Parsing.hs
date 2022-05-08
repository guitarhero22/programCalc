module Parsing (
    Parser (Parser),
    somewith,
    manywith,
    symbol,
    token,
    parse,
    apply,
    parserfail,
    guard,
    space,
    digit,
    none,
    some,
    many,
    paren,
    bracket,
    string,
    char,
    sat,
    (<|>)
)
where
import Data.Char
import qualified Data.Functor as Functor
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad

{- MONAD PARSER -}
----------------------------------------------------------------
newtype Parser a = Parser {runParser :: String -> [(a, String)]}

-- apply parser
apply :: Parser a -> String -> [(a, String)]
apply = runParser

-- parse
parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- parser as a functor
instance Functor.Functor Parser where
    fmap f p = Parser (\s -> [(f x, s')
                            |   (x, s') <- apply p s])

-- parser as an applicative
instance Applicative.Applicative Parser where
    pure x = Parser (\s -> [(x, s)])
    p <*> q = Parser (\s -> [(y x, s'')
                        | (x, s') <- apply q s
                        , (y, s'') <- apply p s'])

-- parser as a monad
instance Monad.Monad Parser where 
    return x = Parser (\s -> [(x, s)])
    p >>= q = Parser (\s -> [(y, s'')
                            |   (x, s') <- apply p s,
                                (y, s'') <- apply (q x) s'])

-- for when parser fails
parserfail :: Parser a
parserfail = Parser $ const []

{- COMBINATORS -}
---------------------------------------------------------

-- extract a single character
getc :: Parser Char
getc = Parser f
        where   f []      = []
                f (c:cs) = [(c, cs)]

-- extract a single character satisfying some condition
sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- getc
        guard (p c)
        return c

-- guard on some condition
guard :: Bool -> Parser ()
guard True = return ()
guard False = parserfail

-- extract a specific character
char :: Char -> Parser ()
char x = do
        c <- sat (==x)
        return ()

-- extract a specific string
string :: String -> Parser ()
string [] = return ()
string (x:xs) = do
                char x
                string xs
                return ()

-- recognizing a lower char
lower :: Parser Char
lower = sat isLower

-- recognizing a digit
digit :: Parser Int
digit = do 
        d <- sat isDigit
        return (fromEnum d - fromEnum '0')

-- alternation operator
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
        where f s = let ps = apply p s in
                    if null ps then apply q s
                    else ps

-- eats up space
space :: Parser ()
space = Monad.void (many (sat isSpace))

symbol :: String -> Parser ()
symbol xs = space >> string xs

-- for defining parsers that ignore whitespace
token :: Parser a -> Parser a
token p = space >> p

-- combinator to repeat a parser one or more times, the (+)
some :: Parser a -> Parser [a]
some p = do
        x <- p
        xs <- many p
        return (x:xs)

-- combinator to get many from some
optional :: Parser [a] -> Parser [a]
optional p = p <|> none

-- empty list in the parser monad
none :: Parser [a]
none = return []

-- combinator to repeat a parser zero or more times, the (*)
many :: Parser a -> Parser [a]
many p = optional (some p)

-- instances of p separated by q
somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do 
                x <- p
                xs <- many (q >> p)
                return (x:xs)

--instances of p separated by q
manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = optional (somewith q p)


-- Parse brackets
bracket :: Parser a -> Parser a
bracket p = do
            symbol "["
            x <- p
            symbol "]"
            return x

-- parse paranthensis
paren :: Parser a -> Parser a
paren p = do
            symbol "("
            x <- p
            symbol ")"
            return x

-- parce braces
brace :: Parser a -> Parser a
brace p = do
            symbol "{"
            x <- p
            symbol "}"
            return x

-- lowers :: Parser String
-- lowers = many lower

--parser for natural numbers
-- natural :: Parser Int
-- natural = token nat
-- nat = do
--         ds <- some digit
--         return (foldl1 shiftl ds)
--         where shiftl m n = 10*m+n

--parser for integer numbers
-- int :: Parser Int
-- int = (do
--         symbol "-"
--         n <- natural
--         return (-n)
--     ) <|> natural
-- parser addition
-- addition :: Parser Int
-- addition = do
--             m <- digit
--             char '+'
--             n <- digit
--             return (m+n)

-- not needed for the actual parser, showing efficiency of alternaton
-- wrong :: Parser Int
-- wrong = digit <|> addition

-- better :: Parser Int
-- better = addition <|> digit

-- best = digit >>= rest
-- rest m = do
--         char '+'
--         n <- digit
--         return (m+n) <|> return m


--parser for a list of ints
-- ints :: Parser [Int]
-- ints = bracket (manywith (symbol ",") int)