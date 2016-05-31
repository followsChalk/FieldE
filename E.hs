module E
where

import Data.List
import Point as Point
import GFp as GFp


a = 8

data E = E {get :: Point GFp} deriving(Eq)
instance  Show E where
    show  = show . E.get 


zero = E [gfp 0,p]

add :: E -> E -> E
add q@(E [x1,y1]) r@(E [x2,y2]) | q==E.zero =   r
                                | r==E.zero =   q
                                | x1/=x2    =   let s   = diw (y1-y2) (x1-x2)
                                                    x3  = s*s -x1-x2
                                                    y3  = y1 + s*(x3-x1)  
                                            in E [x3,-y3]
                                | y1==(-y2) =   E.zero 
                                | True      =   let s = diw ((gfp 3)*x1*x1 + E.a) ((gfp 2)*y1)
                                                    x3  = s*s-(gfp 2)*x1
                                                    y3  = y1 + s*(x3-x1)
                                            in E [x3,-y3]


ainv :: E -> E
ainv a@(E [x,y])    | a == E.zero   = E.zero 
                    | True          = E [x,-y]

sub :: E -> E ->E 
sub q r = E.add q (E.ainv r)

smul :: Int -> E -> E
smul 1 q = q
smul i q = E.add (E.smul (i-1) q) q

instance  Num E where
    (+)         = E.add
    (-)         = E.sub 
    (*)         = (+)
    negate      = E.ainv
    abs         = id
    signum      = const E.zero
    fromInteger i = E [gfp $ div (fromInteger i) (GFp.get p),gfp $ mod (fromInteger i) (GFp.get p)] 


gen = E $ map gfp [0,1]
p'  = E $ map gfp [0,1]
q   = E $ map gfp [3,12]
r   = E $ map gfp [1,2]
alpha = E $ map gfp [0,2]
beta = E $ map gfp [16,8]



gens curr   | curr == next      = curr
            | True              = gens next
    where 
    next = nub $ curr ++ [E.add q r| q<-curr,r<-curr]


toFloatPoint = \(E [GFp x,GFp y]) -> [fromIntegral x,fromIntegral y] 

main::IO()
main =do --putStrLn $ displayLabeled (GFp.get p) (GFp.get p) $ zip  (map toFloatPoint $ gens [gen]) [1..length (gens [gen])]
         --putStrLn $ show $  length (gens [gen])
         putStrLn $ show $  E.smul 6 p'