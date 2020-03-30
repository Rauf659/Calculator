module Calc
    ( runCalc
    ) where

import System.IO

res :: [Float]
res = []

runCalc :: IO ()
runCalc = do
    expr <- getLine
    make expr
        where
            make :: String -> IO ()
            make expr = do
                let slov = words expr 
                if ((slov !! 0) == "printN") then
                    printN (read (slov !! 1)) res
                else
                    printN 0 [(r (calc1 (words expr)))]


{-calculate :: String -> [Float]
calculate e = if (length res) > 10 then res ++ [(calc' e)]
              else res ++ [(calc' e)]
-}

data Expr = Expr {
    r :: Float,
    s :: [String]
}

calc1 :: [String] -> Expr
calc1 (x:xs)
        | (x == "add") = resultadd
        | (x == "sub") = resultsub
        | (x == "mul") = resultmul
        | (x == "div") = resultdiv
        | otherwise = Expr (read x) xs
        where
            a = calc1 xs
            b = calc1 (s a)
            resultadd = Expr ((r a) + (r b)) (s b)
            resultsub = Expr ((r a) - (r b)) (s b)
            resultmul = Expr ((r a) * (r b)) (s b)
            resultdiv = Expr ((r a) / (r b)) (s b)

--add sub 10 add 15 25 
--add sub add 10 10 add 15 25 12 = -8

{- calc :: String -> Float
calc "add" e1 e2 = ( (calc e1) + (calc e2) )
calc "sub" e1 e2 = ( (calc e1) - (calc e2) )
calc "mul" e1 e2 = ( (calc e1) * (calc e2) )
calc "div" e1 e2 = ( (calc e1) / (calc e2) )
calc _ e1 e2 = (trans e1) (trans e2)
-}

printN :: Int -> [Float] -> IO ()
printN _ [] = print "There is no results!"
printN n r | n >= 0 && n <= 9 = print (r !! n)
           | otherwise = print "Position is wrong."

{-trans :: String -> Int
trans num | num == "1" = 1
          | num == "2" = 2
          | num == "3" = 3
          | num == "4" = 4
          | num == "5" = 5
          | num == "6" = 6
          | num == "7" = 7
          | num == "8" = 8
          | num == "9" = 9
          | num == "0" = 0
-}