{-# LANGUAGE GADTs, KindSignatures #-}
module Combinatorics.Symbolic.AllowablConstructions where
import Combinatorics.Symbolic.MathExpr
import Data.Set hiding (map)
    
data Class a where
    Nuetral  :: Class a
    Atomic   :: Set a   -> Class a
    DisUnion :: Class a -> Class a -> Class a
    Cart     :: Class a -> Class a -> Class a
    Sequence :: Class a -> Class a
    PowerSet :: Class a -> Class a
    Multiset :: Class a -> Class a
    Cycle    :: Class a -> Class a

one = Lit 1
sub x y    = x :+: (SumInv y)
divide x y = x :*: (ProductInv y) 

choose n k = (Factorial (Lit n)) `sub` ((Factorial (Lit k)) :*: 
    (Factorial (Lit n `sub` Lit k)))

toGenFunc :: Class a -> MathExpr
toGenFunc Nuetral    = one
toGenFunc (Atomic x) = Var "z"
toGenFunc (DisUnion x y)  = (toGenFunc x) `sub` (toGenFunc y)
toGenFunc (Cart x y)      = (toGenFunc x) :*: (toGenFunc y)
toGenFunc (Sequence x)    = one `divide` (one `sub` (toGenFunc x))
toGenFunc (PowerSet x)    = setize (:+:) x
toGenFunc (Multiset x)    = setize divide x
                                    
setize :: (MathExpr -> MathExpr -> MathExpr) -> Class a -> MathExpr
setize op x = BigProduct (Lit 0) (Lit $ sizeOf x) 
    ((one `sub` (Var "z")) :^: Var "n")  
        :^: (SumInv (App (toGenFunc x) $ Var "n"))

valueChoose n 0 = 1
valueChoose 0 k = 0
valueChoose n k = valueChoose (n-1) (k-1) * n `div` k

sizeOf :: Class a -> Int
sizeOf (Nuetral)      = 1
sizeOf (Atomic x)     = size x
sizeOf (DisUnion x y) = (sizeOf x) + (sizeOf y)
sizeOf (Cart x y)     = (sizeOf x) * (sizeOf y)
sizeOf (Sequence x)   = sum $ map (\k -> (sizeOf x) ^ k) [0..sizeOf x]
sizeOf (PowerSet x)   = sum $ map (valueChoose (sizeOf x)) [0..(sizeOf x)]
sizeOf (Multiset x)   = sum $ map (\k -> valueChoose ((sizeOf x) + k - 1) k) [0..(sizeOf x)]
    
evalGF :: MathExpr -> Int -> MathExpr
evalGF gf n = undefined

distribution :: MathExpr -> (Double -> Double -> Double)
distribution = undefined

--the next thing is to make a isomorphism between the 
--so here is how I do it. The binary tree is pointing out one sequence. The sequence with 2
--unique colors, and visa versus. The binary trees formula is actually a sum expression of
--the general tree expression. This is what we expect. So The operation of pointing is the 
--derivative. 

--or specifically take a binary tree. Divive the number of node options in half.
--rotate nineties degrees and readd the root.

--45 degs I finally get it. Subtract the root, connect rotate. 

--it is taken the derivative at the first part. and summing it up for each location. 

--what if division means derivative in value land. You to remove an object, but it is not 
--clear where. In otherwords any derivative will do. Division doesn't constrain the location
--the derivative is evaluated. Thus since plane trees are always non-empty, zero is always possible.
        
{-

 If every type can be broken into combinatorial class and a atomic set. 
 Then you break into structure and elements. You make isometries between the 
 classes and the you just need to convert the elements.
 
 Reify a type into a combinatorial class you can derive the ismorphism.
 
 My new thought is each type has a formula that it carries, at runtime. 
 that is used to perform the conversion using an isometry

 for instance 
 
 You can make a isometry between a binary tree and a plane tree with the same 
 elements by 
 
 I need the function Class -> Class -> Index -> Index
 
 where Index is an index into a combinatorial class enumeration 
 
 I WAS having trouble with understanding what the definite integral of a derivative
 gives you. It gives you all the changes.
 
-}