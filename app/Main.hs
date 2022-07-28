module Main where

import Expr
import Parse

main :: IO ()
main = print $ eval (BinOp Add (Num 3) (Num 39))
