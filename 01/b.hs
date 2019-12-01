module Main where

import System.Environment

fuelReq x = loop 0 ((div x 3)-2)
    where loop acc x | x <= 0 = acc
                     | otherwise = loop (acc + x) ((div x 3)-2) 

main = do
    x:_ <- getArgs
    s <- readFile x
    let res = sum $ map (fuelReq . read::String->Int) $ lines s
    print res
            