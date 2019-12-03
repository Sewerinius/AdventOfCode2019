module B where

import Data.List.Split (splitOn)
import System.Environment
import Debug.Trace (trace)
import Data.Maybe (catMaybes)

-- Start Stop Level SDistance
data Line = Horizontal Int Int Int Int | Vertical Int Int Int Int deriving (Show)

crosses :: Line -> Line -> Bool
crosses (Horizontal sx ex y _) (Vertical sy ey x _) = between sx x ex && between sy y ey
crosses a@(Vertical _ _ _ _) b@(Horizontal _ _ _ _) = crosses b a
crosses _ _ = False

(<=<) :: Int -> Int -> Int -> Bool
(<=<) a b c = a <= b && b <= c

between :: Int -> Int -> Int -> Bool
between a b c = (<=<) a b c || (<=<) c b a

crossesPoint :: Line -> Line -> Maybe (Int, Int, Int)
crossesPoint x y = case crosses x y of 
    False -> Nothing
    True -> Just $ crossingPoint x y
    where 
        crossingPoint (Horizontal sx _ y dy) (Vertical sy _ x dx) = (x, y, dx + abs (x - sx) + dy + abs (y - sy))
        crossingPoint a@(Vertical _ _ _ _) b@(Horizontal _ _ _ _) = crossingPoint b a

findCrossingPoints :: [Line] -> [Line] -> [(Int, Int, Int)]
findCrossingPoints xs ys = catMaybes [crossesPoint x y | x <- xs, y <- ys]

toLines :: [String] -> [Line]
-- toLines xs | trace ("toLines " ++ show xs) False = undefined 
toLines xs = tLLoop 0 0 xs 0 [] 
    where 
        tLLoop :: Int -> Int -> [String] -> Int -> [Line] -> [Line]
        -- tLLoop x y xs os | trace ("tLLoop " ++ show x ++ " " ++ show y ++ " " ++ show xs) False = undefined 
        tLLoop _ _ [] _ acc = acc
        tLLoop x y (i:is) dist acc = do
            let (l, x2, y2, dist2) = toLine x y i dist
            tLLoop x2 y2 is dist2 (l:acc) 

toLine :: Int -> Int -> String -> Int -> (Line, Int, Int, Int)
-- toLine x y (d:_) | trace ("toLine " ++ show x ++ " " ++ show y ++ " " ++ show d) False = undefined 
toLine x y (d:ds) dist = do
    let len = read ds :: Int
    case d of 
        'U' -> (Vertical y (y+len) x dist, x, (y+len), dist + len)
        'D' -> (Vertical y (y-len) x dist, x, (y-len), dist + len)
        'L' -> (Horizontal x (x-len) y dist, x-len, y, dist + len)
        'R' -> (Horizontal x (x+len) y dist, x+len, y, dist + len)

-- fun :: [String] -> [String] -> Int
fun xs ys = minimum $ map (\(_, _, d) -> d) $ filter (\(x , y, _) -> x /= 0 || y /= 0) $ findCrossingPoints (toLines xs) (toLines ys)
-- 

fromFile :: String -> IO ()
fromFile x = do
    s <- readFile x
    let [sa, sb] = map (splitOn ",") $ lines s
    print $ show (toLines sa) ++ show (toLines sb)
    print $ fun sa sb

main = do
    x:_ <- getArgs
    fromFile x
        
    