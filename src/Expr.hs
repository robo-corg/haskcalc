module Expr where

data BinOp = Add | Sub | Mul
    deriving (Eq, Show)

data UnaryOp = Negate | Abs | Signum
    deriving (Eq, Show)

data Expr =
    Var String |
    Num Integer | BinOp BinOp Expr Expr | UnaryOp UnaryOp Expr
    deriving (Eq, Show)

instance Num Expr where
  (+) = BinOp Add
  (*) = BinOp Mul
  abs = UnaryOp Abs
  signum = UnaryOp Signum
  fromInteger = Num
  negate = UnaryOp Negate

binOpFun :: Num a => BinOp -> (a -> a -> a)
binOpFun Add = (+)
binOpFun Sub = (-)
binOpFun Mul = (*)

data MaybeReduced a = Reduced a | Unchanged a
    deriving (Eq, Show)

unwrap :: MaybeReduced a -> a
unwrap (Reduced a) = a
unwrap (Unchanged a) = a

instance Functor MaybeReduced where
  fmap f (Reduced a) = Reduced (f a)
  fmap f (Unchanged a) = Unchanged (f a)

instance Applicative MaybeReduced where
  pure = Unchanged
  (<*>) (Reduced f) a = Reduced $ f $ unwrap a
  (<*>) f (Reduced a) = Reduced $ (unwrap f) a
  (<*>) f a = Unchanged $ (unwrap f) (unwrap a)

instance Monad MaybeReduced where
  (>>=) (Reduced a) f = Reduced $ unwrap $ f a
  (>>=) (Unchanged a) f = f a

reduceOne :: Expr -> MaybeReduced Expr
reduceOne (BinOp op (Num lhs) (Num rhs)) = Reduced $ Num $ binOpFun op lhs rhs
reduceOne (BinOp op lhs rhs) = do
    lhs_reduced <- reduceOne lhs
    rhs_reduced <- reduceOne rhs
    return (binOpFun op lhs_reduced rhs_reduced)

reduceOne (UnaryOp Negate (Num n)) = Reduced $ Num $ -n
reduceOne (UnaryOp Abs (Num n)) = Reduced $ Num $ abs n
reduceOne (UnaryOp Signum (Num n)) = Reduced $ Num $ signum n

reduceOne (UnaryOp op expr) = do
    arg_reduced <- reduceOne expr
    return (UnaryOp op $ arg_reduced)

reduceOne irreducable = Unchanged irreducable

reduceAllInner :: MaybeReduced Expr -> MaybeReduced Expr
reduceAllInner (Reduced expr) = reduceAllInner (reduceOne expr)
reduceAllInner unchanged = unchanged

reduceAll :: Expr -> Expr
reduceAll expr = unwrap $ reduceAllInner $ Reduced expr