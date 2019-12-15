module A where

import Data.Map (Map, singleton, fromList, fromAscList, (!), (!?), insert)
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, member, insert)
import Data.List.Extra (trim, splitOn)
import System.Environment
import Debug.Trace (trace)

data Recipe = Recipe (Int, String) [(Int, String)] deriving (Show)

toTuple :: String -> (Int, String)
toTuple a = let
    [c, n] = splitOn " " a
    in (read c::Int, n)

fromStrings :: String -> String -> Recipe
fromStrings resS indS = 
    let
        indL = map trim $ splitOn "," indS
        indT = map toTuple indL
        resT = toTuple resS
    in Recipe resT indT

fromFile x = do
    s <- readFile x
    return $ map ((\[a, b] -> fromStrings b a) . map trim . splitOn "=>") $ lines s

toMap :: [Recipe] -> Map String Recipe
toMap xs = fromList $ map (\x@(Recipe (_, n) _) -> (n, x)) xs

topologicalSort :: Map String Recipe -> Set String -> String -> ([String], Set String)
-- topologicalSort m visited s | trace ("TopologicalSort " ++ s) False = undefined 
topologicalSort m visited s | member s visited = ([], visited)
                            | otherwise = 
    let 
        visited2 = Data.Set.insert s visited
        nexts = filter (/="ORE") $ map snd $ (\(Recipe _ xs) -> xs) $ m ! s 
        loop m visited3 [] acc = (acc, visited3)
        loop m visited3 (s2:ss) acc = let
            (newAcc, newVisited) = topologicalSort m visited3 s2
            in loop m newVisited ss (newAcc ++ acc)
        (newAcc, newVisited) = loop m visited2 nexts []
    -- in trace ("TSRes: " ++ show (s:newAcc, newVisited)) (s:newAcc, newVisited)
    in (s:newAcc, newVisited)
            
getOreValue :: Map String Recipe -> String -> Int
getOreValue m s = let
    (ts, _) = topologicalSort m empty s
    loop :: Map String Int -> [String] -> Int
    loop m2 [] = m2 ! "ORE"
    loop m2 (x:xs) = let
        Recipe (c, _) nexts = m ! x
        mult = ceiling $ fromIntegral (m2 ! x) / fromIntegral c
        loop2 :: Map String Int -> [(Int, String)] -> Map String Int
        loop2 m3 [] = m3
        loop2 m3 ((nextVal, nn):ns) = loop2 (Data.Map.insert nn (nextVal*mult + fromMaybe 0 (m2 !? nn)) m3) ns
        in loop (loop2 m2 nexts) xs
    in loop (singleton s 1) ts

main = do
    v <- fromFile "in.txt"

    -- print $ count ((==2) . snd) $ cnv contents
    print $ getOreValue (toMap v) "FUEL"
