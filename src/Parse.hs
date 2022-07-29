module Parse where

import Expr
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Text.Parsec.Expr
import Control.Monad (void)
import Text.Parsec.Token

-- Non-token based parser which seems to work:
-- whitespace :: Parser ()
-- whitespace = void $ many $ oneOf " \n\t"

-- lexeme :: Parser a -> Parser a
-- lexeme p = do
--            x <- p
--            whitespace
--            return x

-- integer :: Parser Integer
-- integer = read <$> lexeme (many1 digit)

-- intExpr :: Parser Expr
-- --intExpr = Num `fmap` integer
-- intExpr = do
--     a <- integer
--     return (Num a)

-- term :: Parser Expr
-- term    = intExpr <?> "simple expression"



-- Text.Parsec.Token based approach:

-- expr :: Parser Expr
-- expr    = buildExpressionParser table term
--         <?> "expression"

-- parseExpr :: String -> ParseError
-- parseExpr s = parse "" s

-- term    =  parens expr
--         <|> Num <$> integer
--         <?> "simple expression"

term    =  Num <$> integer
        <?> "simple expression"


-- table :: [[Operator String u m Expr]]
-- table   = [ [prefix "-" negate, prefix "+" id ]
--         , [postfix "++" (+1)]
--         , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
--         , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
--         ]

--binary :: GenTokenParser s1 u1 m1 -> (a -> a -> a) -> Assoc -> Operator s2 u2 m2 a
-- binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
-- --prefix :: GenTokenParser s1 u1 m1 -> (a -> a) -> Operator s2 u2 m2 a
-- prefix  name fun       = Prefix (do{ reservedOp name; return fun })
-- postfix name fun       = Postfix (do{ reservedOp name; return fun })

