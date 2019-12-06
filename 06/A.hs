module A where

import Data.List.Split (splitOn)
import System.Environment
import Debug.Trace (trace)

-- import Data.MultiMap (MultiMap, fromList)
import Data.Map (Map, fromList, (!?), keys)


calc :: Map String String -> String -> Int
calc m s = case m !? s of
            Nothing -> 0
            Just x -> calc m x + 1

prog x = do
    let m = fromList x
    print $ sum $ map (calc m) $ keys m

fromFile x = do
    s <- readFile x
    let sa = map ((\[x,y] -> (y,x)) . splitOn ")") $ lines s
    prog sa

main = do
    x:_ <- getArgs
    fromFile x

