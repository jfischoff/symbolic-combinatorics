{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Combinatorics.Symbolic.MathExpr
import Language.LBNF.Runtime 
import Prelude hiding (exp)
import Combinatorics.Symbolic.MathExpr.Evaluator

h = eval [] [expr| log 3.0 * exp 1.2 + 4.0 - 3.0 / (sigma 3.0 12.0 n) |]

s = eval [] [expr| sigma 3.0 9.0 n |]

p = eval [] [expr| pi 3.0 9.0 (2.0 * n) |]

fact = eval [] [expr| 5.0! |]
