module B where

import Data.Char (digitToInt)
import System.Environment
import Debug.Trace (trace)
import qualified Data.Vector as V (Vector, fromList, (!), length)
import Data.Maybe (fromMaybe)

-- TODO:
-- Awfully slow, takes a couple of minutes when compiled
-- Look into what is actually happening
-- Knowing how far inside is the result, we could skip the previous part

lastDigit x = mod (abs x) 10

mkSums :: [Int] -> V.Vector Int
mkSums xs = V.fromList $ scanl1 (+) xs

partialSum :: V.Vector Int -> Int -> Int -> Int
-- <s; s+n)
partialSum v s n = 
    let 
        len = V.length v
        ia = (s+n-1)
        ib = (s-1)
        a | ia >= len = v V.! (len-1)
            | ia < 0 = 0
            | otherwise = v V.! ia
        b | ib >= len = v V.! (len-1)
            | ib < 0 = 0
            | otherwise = v V.! ib
    in a - b 

run :: Int -> [Int] -> [Int]
run n xs | trace ("run n = " ++ show n ++ show (take 7 xs)) False = undefined
-- run n xs | trace ("run n = " ++ show n ++ show xs) False = undefined
run 0 xs = xs
run n xs = run (n-1) $ step xs

step :: [Int] -> [Int]
step xs = loop 1 xs []
    where
        sums = mkSums xs
        loop _ [] acc = reverse acc
        loop n l@(_:xs) acc = loop (n+1) xs ((lastDigit $ calc n sums) : acc)

calc :: Int -> V.Vector Int -> Int
calc n sums = loop (n-1) 0
    where 
        len = V.length sums
        loop i acc | i >= len = acc
                    | otherwise = loop (4 * n + i) (acc + (partialSum sums i n) - (partialSum sums (i+2*n) n))

fromFile x = do
    s <- readFile x
    return $ map digitToInt s

toInt :: [Int] -> Int
toInt = foldl (\a b -> a * 10 + b) 0

main = do
    -- x:_ <- getArgs
    v <- fromFile "in.txt"
    let l = length v
    let v2 = take (10000*l) $ cycle v
    let v3 = run 100 v2
    let skip = toInt $ take 7 v
    -- print skip
    putStrLn $ concatMap show $ take 8 $ drop skip v3
    -- putStrLn $ concatMap show $ take 8 $ run 100 v

