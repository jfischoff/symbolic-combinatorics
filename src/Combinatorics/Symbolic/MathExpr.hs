{-# LANGUAGE GADTs, KindSignatures, TypeOperators #-}
module Combinatorics.Symbolic.MathExpr where
type Name = String


      
data MathExpr where
    Infinity       :: MathExpr
    Lit            :: Int      -> MathExpr
    (:+:)          :: MathExpr -> MathExpr -> MathExpr
    SumInv         :: MathExpr -> MathExpr
    (:*:)          :: MathExpr -> MathExpr -> MathExpr
    ProductInv     :: MathExpr -> MathExpr
    Log            :: MathExpr -> MathExpr
    Expon          :: MathExpr -> MathExpr
    (:^:)          :: MathExpr -> MathExpr -> MathExpr 
    Sigma          :: MathExpr -> MathExpr -> MathExpr -> MathExpr
    BigProduct     :: MathExpr -> MathExpr -> MathExpr -> MathExpr
    Lim            :: Name     -> MathExpr
    Derivative     :: Name     -> MathExpr -> MathExpr
    AntiDerivative :: Name     -> MathExpr -> MathExpr
    Factorial      :: MathExpr -> MathExpr
    Function       :: [Name]   -> MathExpr -> MathExpr
    App            :: MathExpr -> MathExpr -> MathExpr
    Var            :: Name     -> MathExpr
    
    

    
--I should make a quasi quoter for this 
    