{-# LANGUAGE GADTs, KindSignatures, TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Combinatorics.Symbolic.MathExpr where
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;
    
EPlus.      Expr0     ::= Expr1 "+" Expr1 ;
ESub.       Expr1     ::= Expr1 "-" Expr2 ;
ETimes.     Expr2     ::= Expr2 "*" Expr3 ;
EDivides.   Expr3     ::= Expr3 "/" Expr4 ;
ELog.       Expr4     ::= "log" Expr5 ;
EExp.       Expr5     ::= "exp" Expr6 ;
EFact.      Expr6     ::= Expr7 "!" ;
ERaise.     Expr7     ::= Expr7 "^" Expr8 ;
ESigma.     Expr8     ::= "sigma" Expr8 Expr9 Expr9 ;
EPi.        Expr9     ::= "pi" Expr9 Expr10 Expr10 ;
EInfinity.  Expr10    ::= "infinity";
EVar.       Expr11    ::= Ident;
EDouble.    Expr12    ::= Double;


_.  Expr      ::= Expr0 ;
_.  Expr0     ::= Expr1 ;
_.  Expr1     ::= Expr2 ;
_.  Expr2     ::= Expr3 ;
_.  Expr3     ::= Expr4 ;
_.  Expr4     ::= Expr5 ;
_.  Expr5     ::= Expr6 ;
_.  Expr6     ::= Expr7 ;
_.  Expr7     ::= Expr8 ;
_.  Expr8     ::= Expr9 ;
_.  Expr9     ::= Expr10 ;
_.  Expr10    ::= Expr11 ;
_.  Expr11    ::= Expr12 ;
_.  Expr12    ::= "(" Expr ")" ;    

    
TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Expr ;
  |]    
  
{-

Lim            :: Name     -> MathExpr
Derivative     :: Name     -> MathExpr -> MathExpr
AntiDerivative :: Name     -> MathExpr -> MathExpr
Factorial      :: MathExpr -> MathExpr
Function       :: [Name]   -> MathExpr -> MathExpr
App            :: MathExpr -> MathExpr -> MathExpr




-}




