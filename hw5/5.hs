{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import Control.Applicative
import Expr
import ExprT
import Parser
import qualified StackVM as VM

instance Expr VM.Program where
  lit a = [VM.PushI a]
  add p1 p2 = p1 ++ p2 ++ [VM.Add]
  mul p1 p2 = p1 ++ p2 ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

exec :: String -> Maybe (Either String VM.StackVal)
exec s = VM.stackVM <$> compile s
