module Parse where

import Expr
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad (void)


lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
integer     = P.integer lexer

reservedOp = P.reservedOp lexer

expr :: Parser Expr
expr    = buildExpressionParser table term
        <?> "expression"

term :: Parser Expr
term    =  parens expr
        <|> Num <$> integer
        <?> "simple expression"

table   = [ [prefix "-" negate, prefix "+" id ]
        , [postfix "++" (+1)]
        , [binary "*" (*) AssocLeft] --, binary "/" (div) AssocLeft ]
        , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
        ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

parseExpr s = parse expr "" s