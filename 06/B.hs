module B where

import Data.List.Split (splitOn)
import System.Environment

-- import Data.MultiMap (MultiMap, fromList)
import Data.Map (Map, fromList, (!?), keys)

diffLen :: [String] -> [String] -> Int
diffLen (x:xs) (y:ys) = if x == y then diffLen xs ys else length xs + length ys

trace :: Map String String -> String -> [String] -> [String]
trace m s acc = 
    case m !? s of
        Nothing -> s:acc
        Just x -> trace m x (s:acc)

calc :: Map String String -> String -> Int
calc m s = 
    case m !? s of
        Nothing -> 0
        Just x -> calc m x + 1

prog x = do
    let m = fromList x
    print $ diffLen (trace m "YOU" []) (trace m "SAN" [])

fromFile x = do
    s <- readFile x
    let sa = map ((\[x,y] -> (y,x)) . splitOn ")") $ lines s
    prog sa

main = do
    x:_ <- getArgs
    fromFile x
    
    