module Main where

import System.Environment

main = do
    x:_ <- getArgs
    s <- readFile x
    let res = sum $ map (flip (-) 2 . (`div` 3) . read::String->Int) $ lines s
    print res
        