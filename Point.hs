module Point
    (
        Point,
        zero,
        add,
        smul,
        dist,
        mean,
        pointExample,
        display,
        displayLabeled
    )
where

import Data.List
import Data.Function

----Description------------------------------------------
---------------------------------------------------------
-- Point = ( a X a X ... X a , + , smul )



----Examples---------------------------------------------
---------------------------------------------------------

x::Point Float;
x=[1,1]
y=[2,3]
xs=[x,y,[5,2]]
xss=[xs,[[1,3]]]

pointExample :: String
pointExample    = "x=\t"++show x++"\n"
                ++"y=\t"++show y++"\n"
                ++"add x y=\n"++show (add x y)++"\n"
                ++"xs=\t"++show xs ++"\n"
                ++"mean xs=\n" ++show (mean xs)++"\n"


main::IO()
main = do 
        putStrLn pointExample
        putStrLn $ display 6 6 xss


----Definition-------------------------------------------
---------------------------------------------------------

type Point f  = [f]

zero = repeat 0;

add :: (Num f) => Point f -> Point f -> Point f
add = zipWith (+) 

smul :: (Num f) => f -> Point f -> Point f
smul a v = map (a*) v
 
dist ::  Point Float -> Point Float -> Float
dist v u = sqrt $ sum $ fmap (^2) $ zipWith (-) v u

mean :: [Point Float] -> Point Float
mean xs = smul (1.0/fromIntegral (length xs)) $ foldl1 add xs




----Display----------------------------------------------
---------------------------------------------------------

--display resolutionX resolutionY
display :: Int -> Int -> [[Point Float]] -> String
display n m vss = displayLabeled n m (label vss) 
    
displayLabeled :: Int -> Int -> [(Point Float,Int)] -> String
displayLabeled n m ps = displayLabeled' "" $ 
                            sortBy   (\([x1,y1],l1) ([x2,y2],l2) -> compare [y1,x1] [y2,x2]) $
                            nubBy    ((==) `on` fst) $
                                            (roundLabeledPts ps) ++ 
                                            [([x,y],0) | y<-[0..m-1],x<-[0..n-1]]
    where
    displayLabeled' ret []                 = ret
    displayLabeled' ret (([x,y],l):ps)     = displayLabeled'   (ret++
                                                                (if x==(n-1) 
                                                                    then "\n"
                                                                    else showLabel l
                                                                )) ps


showLabel :: Int -> String
showLabel 0 = "| "
showLabel i = "|o"
{-showLabel 2 = "|x"
showLabel 3 = "|+"
showLabel 4 = "|*"
showLabel i = "|"++show i
-}
roundLabeledPt :: (Point Float,Int) ->  (Point Int,Int)
roundLabeledPt p = (map round (fst p),snd p)  

roundLabeledPts:: [(Point Float,Int)] ->  [(Point Int,Int)]
roundLabeledPts = map roundLabeledPt

label :: [[a]] -> [(a,Int)]
label xss= concat $ map (\(xs,l) -> map (\x -> (x,l)) xs) $ zip xss [1..length xss] 

--n=sirina 
--bufferPoints :: Int  -> [Point Int] -> [Int] 
--bufferPoints n vs = nub $ map (\[x,y] -> y*n+x) vs 
