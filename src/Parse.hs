module Parse where

import Expr
import Data.Char


data Parsed a = Parsed a [Token]
type ParseResult a = Either ParseError (Parsed a)

instance Functor Parsed where
    fmap f (Parsed a tokens) = Parsed (f a) tokens


type Parser a = [Token] -> Parsed a

p2 :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
p2 pa pb f tokens =
    let (Parsed a a_remain) = (pa tokens) in
        let (Parsed b b_remain) = (pb a_remain) in
            Parsed (f a b) b_remain

p :: Parser a  -> (a -> c) -> Parser c
p pa f tokens =
    let (Parsed a a_remain) = (pa tokens) in
        Parsed (f a) a_remain

ap :: [Token] -> Parsed Expr
ap tokens = Parsed (Num 42) tokens


combined :: [Token] -> Parsed Expr
combined = p ap id


-- instance Applicative Parser where
--   pure a = Parsed a []
--   (<*>) = _



data Token = NumTok Int | BinOpTok BinOp

tokenizeNum :: String -> String -> (String, Token)
tokenizeNum digits (ch:rest)
    | isDigit ch = tokenizeNum (digits ++ [ch]) rest
    | otherwise = ([ch], NumTok $ read digits)
tokenizeNum digits [] = ([], NumTok $ read digits)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':rest) = tokenize rest
tokenize ('+':rest) = (BinOpTok Add) : tokenize rest
tokenize ('-':rest) = (BinOpTok Sub) : tokenize rest
tokenize ('*':rest) = (BinOpTok Mul) : tokenize rest
tokenize (ch:rest)
    | isDigit ch = let (unused, tok) = tokenizeNum [ch] rest in
        tok : tokenize (unused ++ rest)

data ParseError = UnknownParseError
    deriving (Eq, Show)



negateExpr :: Expr -> Expr
negateExpr (Num a) = Num (-a)
negateExpr expr = UnaryOp Negate expr

parseExprTok :: [Token] -> Either ParseError Expr

pmap :: (Expr -> Expr) -> ParseResult Expr -> ParseResult Expr
pmap _ (Left err) = Left err
pmap f (Right (Parsed expr remaining)) = Right $ Parsed (f expr) remaining

parseAnyTok :: [Token] -> [Token] -> ParseResult Token
parseAnyTok allowed_tokens (tok:tokens) = if elem tok allowed_tokens then
    Right $ Parsed tok tokens
    else
    Left UnknownParseError
parseAnyTok _ [] = Left UnknownParseError

parseExprTermsTok :: [Token] -> ParseResult Expr
parseExprTermsTok s = (parseTermTok s) <*> (parseAnyTok [BinOpTok Add, BinOpTok Sub])
--parseExprTermsTok s = (parseTermTok s) <*> (parseAnyTok [BinOpTok Add, BinOpTok Sub])


-- parseExprTermsTok s = case parseTermTok s of
--     Right (term, remaining) -> Right (case remaining of
--             ((BinOpTok Add):remaining) -> pmap (BinOp Add term) (parseExprTermsTok remaining)
--         )
--     Left err -> Left err

parseExprTok ((BinOpTok Sub):rest) = fmap negateExpr (parseExprTok rest)
parseExprTok ((NumTok lhs):(BinOpTok op):rest) =
    fmap (BinOp op (Num lhs)) (parseExprTok rest)

parseTermTok :: [Token] -> ParseResult Expr
parseTermTok [NumTok n] = Right (Num n)

parseExpr :: String -> Either ParseError Expr
parseExpr s = parseExprTok $ tokenize s