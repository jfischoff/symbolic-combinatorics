{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Combinatorics.Symbolic.MathExpr.Evaluator where
import Combinatorics.Symbolic.MathExpr
import Language.LBNF.Runtime -- overloaded pretty-printing function
import Prelude    
    
eval vs = eval' where
  varval v = maybe (error $ "undefined variable" ++ printTree v) id $ flip lookup vs v
  eval' [expr| [:a:] + [:b:]          |] = eval' a + eval' b
  eval' [expr| [:a:] - [:b:]          |] = eval' a - eval' b
  eval' [expr| [:a:] * [:b:]          |] = eval' a * eval' b
  eval' [expr| [:a:] / [:b:]          |] = eval' a / eval' b
  eval' [expr| log [:a:]              |] = log (eval' a)
  eval' [expr| exp [:a:]              |] = exp (eval' a)
  eval' [expr| [:a:] !                |] = factorial (eval' a)
  eval' [expr| sigma [:a:] [:b:] [:c:]|] = sum     $ map (\x -> eval ((Ident "n", x):vs) c) [(eval vs a) .. (eval vs b)]
  eval' [expr| pi [:a:] [:b:] [:c:]   |] = product $ map (\x -> eval ((Ident "n", x):vs) c) [(eval vs a) .. (eval vs b)]
  eval' [expr| [Double :n:]           |] = n
  eval' [expr| [Ident  :v:]           |] = varval v

factorial 0 = 1
factorial n = n * (factorial (n - 1))