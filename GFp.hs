module GFp
where

import Data.List

prims :: Int -> [Int]
prims p = [ d | d<-[2..p `div` 2], p `mod` d ==0,  prims d == []]

isPrime =  (== []) . prims

extendedGCD :: Int -> Int -> [Int]
extendedGCD m n = extendedGCD'  0 m 1 0 0 n 0 1
    where
    extendedGCD' q_ r_ s_ t_ q r s t    | r==0      = [r_,s_,t_]    
                                        | otherwise = extendedGCD' q r s t  (r_ `div` r) (r_ `mod` r) (s_ - s *(r_ `div` r)) (t_ - t *(r_ `div` r))



p = GFp 391;
k = 2;

data GFp = GFp {get::Int}  deriving(Eq)

instance  Show GFp where
    show = show . get 

gfp :: Int -> GFp
gfp x = GFp $ mod x (get p)

add :: GFp -> GFp -> GFp
add x y = gfp $ get x + get y

ainv :: GFp -> GFp
ainv x = gfp $ get p - get x

sub :: GFp -> GFp -> GFp
sub x y = add x (ainv y)

zero :: GFp 
zero = gfp 0



mul :: GFp -> GFp -> GFp
mul x y = gfp $ get x * get y

minv :: GFp -> GFp
minv =  gfp . ((get p)+) . (!!2) . (extendedGCD (get p)) . get

diw :: GFp -> GFp -> GFp
diw x y = mul x (minv y)

one :: GFp
one = gfp 1



minvTable = [minv (gfp x) | x <- [0..get p - 1]]

instance  Num GFp where
    (+)     = add
    (-)     = sub 
    (*)     = mul
    negate  = ainv 
    abs     = id
    signum  = const one
    fromInteger = gfp . fromIntegral



{-
type GFq = [GFp]

(+..) :: GFq -> GFq -> GFq 
x +.. y = map (uncurry (+.)) $ zip x y 

--(*..) :: GFq -> GFq -> GFq
--x *.. y = ( map x) `qMod` g
-} 

 
--main :: IO()
--main =  do
 --       putStrLn $ show $ minv 2 