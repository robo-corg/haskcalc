module Expr where

data BinOp = Add | Sub | Mul
    deriving (Eq, Show)

data UnaryOp = Negate
    deriving (Eq, Show)

data Expr =
    Num Int | BinOp BinOp Expr Expr | UnaryOp UnaryOp Expr
    deriving (Eq, Show)

-- instance Eq Expr where
--   Num a == Num b = a == b
--   BinOp op a b ==



binOpFun :: Num a => BinOp -> (a -> a -> a)
binOpFun Add = (+)
binOpFun Sub = (-)
binOpFun Mul = (*)

eval :: Expr -> Int
eval (Num n) = n
eval (BinOp op lhs rhs) = binOpFun op (eval lhs) (eval rhs)
eval (UnaryOp Negate expr) = - (eval expr)