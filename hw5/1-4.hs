module Calc where

import Control.Applicative
import Expr
import ExprT
import Parser


eval :: ExprT -> Integer
eval e =
  case e of
    Lit n -> n
    Add e1 e2 -> eval e1 + eval e2
    Mul e1 e2 -> eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s


instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a+b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a+b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
