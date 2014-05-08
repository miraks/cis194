{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Applicative
import Expr
import Parser
import qualified Data.Map as M


class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add e1 e2 m = (+) <$> e1 m <*> e2 m
  mul e1 e2 m = (*) <$> e1 m <*> e2 m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
