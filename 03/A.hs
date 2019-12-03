module A where

import Data.List.Split (splitOn)
import System.Environment
import Debug.Trace (trace)
import Data.Maybe (catMaybes)

data Line = Horizontal Int Int Int | Vertical Int Int Int deriving (Show)

crosses :: Line -> Line -> Bool
crosses (Horizontal sx ex y) (Vertical sy ey x) = between sx x ex && between sy y ey
crosses a@(Vertical _ _ _) b@(Horizontal _ _ _) = crosses b a
crosses _ _ = False

(<=<) :: Int -> Int -> Int -> Bool
(<=<) a b c = a <= b && b <= c

between :: Int -> Int -> Int -> Bool
between a b c = (<=<) a b c || (<=<) c b a

crossesPoint :: Line -> Line -> Maybe (Int, Int)
crossesPoint x y = case crosses x y of 
    False -> Nothing
    True -> Just $ crossingPoint x y
    where 
        crossingPoint (Horizontal _ _ y) (Vertical _ _ x) = (x, y)
        crossingPoint a@(Vertical _ _ _) b@(Horizontal _ _ _) = crossingPoint b a

findCrossingPoints :: [Line] -> [Line] -> [(Int, Int)]
findCrossingPoints xs ys = catMaybes [crossesPoint x y | x <- xs, y <- ys]

toLines :: [String] -> [Line]
-- toLines xs | trace ("toLines " ++ show xs) False = undefined 
toLines xs = tLLoop 0 0 xs [] 
    where 
        tLLoop :: Int -> Int -> [String] -> [Line] -> [Line]
        -- tLLoop x y xs os | trace ("tLLoop " ++ show x ++ " " ++ show y ++ " " ++ show xs) False = undefined 
        tLLoop _ _ [] os = os
        tLLoop x y (i:is) os = do
            let (l, x2, y2) = toLine x y i
            tLLoop x2 y2 is (l:os) 

toLine :: Int -> Int -> String -> (Line, Int, Int)
-- toLine x y (d:_) | trace ("toLine " ++ show x ++ " " ++ show y ++ " " ++ show d) False = undefined 
toLine x y (d:ds) = do
    let len = read ds :: Int
    case d of 
        'U' -> (Vertical y (y+len) x, x, (y+len))
        'D' -> (Vertical y (y-len) x, x, (y-len))
        'L' -> (Horizontal x (x-len) y, x-len, y)
        'R' -> (Horizontal x (x+len) y, x+len, y)

-- fun :: [String] -> [String] -> Int
fun xs ys = minimum $ map (\(x, y) -> abs x + (abs y)) $ filter (\(x , y) -> x /= 0 || y /= 0) $ findCrossingPoints (toLines xs) (toLines ys)
-- 

fromFile x = do
    s <- readFile x
    let [sa, sb] = map (splitOn ",") $ lines s
    print $ fun sa sb

main = do
    x:_ <- getArgs
    fromFile x
    
