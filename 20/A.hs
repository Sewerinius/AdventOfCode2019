{-# LANGUAGE TupleSections #-}
module A where

import Data.Char (toUpper, isUpper)
import Data.List.Index (indexed)
import Data.List.Split (splitOn)
import Data.Map as Map (Map, fromAscList, (!), insert, (!?), member, empty, filter, toList, lookupMin, delete)
import qualified Data.Set as Set -- (Set, member, notMember, insert, singleton, empty)
import qualified Data.PSQueue as PSQ -- (PSQ)
import Data.Sequence (Seq, viewl, ViewL(..), fromList, (><), (<|))
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment
import Debug.Trace (trace)

nextNeighbours :: Set.Set (Int, Int) -> (Int, Int) -> (Set.Set (Int, Int), [(Int, Int)])
nextNeighbours visited (x, y) = let
    neighbours = [(x, y+1),(x, y-1),(x-1, y),(x+1, y)]
    notVisited = Prelude.filter (\pos -> Set.notMember pos visited) neighbours
    newVisited = foldl (flip Set.insert) visited notVisited
    in (newVisited, notVisited)

checkNeighbour :: Map (Int, Int) Char -> ([((Int, Int), (Int, Int))], (Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
checkNeighbour map' (portals, start, end) myPos neighPos | up && myPos == start = Nothing 
                                                         | up && myPos == end = Just neighPos 
                                                         | up = Just pOut
                                                         | c == '#' = Nothing
                                                         | otherwise = Just neighPos
    where 
        c = get map' neighPos
        up = isUpper c
        [pOut] = map snd $ Prelude.filter (\(pPos, _) -> pPos == myPos) portals

bfs :: ([((Int, Int), (Int, Int))], (Int, Int), (Int, Int)) -> Map (Int, Int) Char -> Int
bfs a@(portals, start, end) map' = loop newVisitedPoss iQueue
    where 
        visitedPoss = Set.singleton start
        (newVisitedPoss, nextPoss) = nextNeighbours visitedPoss start
        iQueue = fromList $ map (, 1) $ mapMaybe (checkNeighbour map' a start) nextPoss
        loop :: Set.Set (Int, Int) -> Seq ((Int, Int), Int) -> Int
        loop visited queue = let
            (pos, dist) :< rest = viewl queue
            out = get map' pos
            (newVisited, nextPoss) = nextNeighbours visited pos
            newComps = map (, dist+1) $ mapMaybe (checkNeighbour map' a pos) nextPoss
            newQueue = rest >< fromList newComps
            trac = trace ("bfsTrac -> pos=" ++ show pos ++ ", dist=" ++ show dist ++ ", out=" ++ show out ++ ", queue=" ++ show rest)
            in if pos == end 
                then dist
                else 
                    case trac out of 
                    --  case out of
                    '#' -> loop visited rest
                    '.' -> loop newVisited newQueue
                    _ -> error "bfs shouldn't get here"
                            -- case map snd $ Prelude.filter (\(pPos, _) -> pPos == pos) portals of 
                            -- [] -> loop visited rest
                            -- [pOut] -> loop (Set.insert pOut visited) ((pOut, dist) <| rest)

get :: Ord a => Map a Char -> a -> Char
get m k = fromMaybe '#' $ m !? k

fromLine :: Int -> Int -> String -> Map (Int, Int) Char -> Map (Int, Int) Char
fromLine _ _ [] acc = acc
fromLine y x (' ':ss) acc = fromLine y (x+1) ss acc
fromLine y x (c:ss) acc = fromLine y (x+1) ss $ insert (x,y) c acc

fromLines :: Int -> [String] -> Map (Int, Int) Char -> Map (Int, Int) Char
fromLines _ [] acc = acc
fromLines y (line:lines) acc = fromLines (y+1) lines $ fromLine y 0 line acc

fromFile x = do
    s <- readFile x
    return $ fromLines 0 (lines s) empty

getPortal :: Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> (String, (Int, Int)) 
getPortal map' p1@(x1, y1) p2@(x2, y2) | p1 > p2 = getPortal map' p2 p1
                                        | x1 == x2 = let
                                        [endpoint] = Prelude.filter (\pos -> (=='.') $ fromMaybe '#' $ map' !? pos) [(x1, y1-1), (x2, y2+1)]
                                        name = [map' ! p1, map' ! p2]
                                        in (name, endpoint)
                                        | y1 == y2 = let
                                        [endpoint] = Prelude.filter (\pos -> (=='.') $ fromMaybe '#' $ map' !? pos) [(x1-1, y1), (x2+1, y2)]
                                        name = [map' ! p1, map' ! p2]
                                        in (name, endpoint)

getPortals :: Map (Int, Int) Char -> ([((Int, Int), (Int, Int))], (Int, Int), (Int, Int))
getPortals map' = loop chars' (Map.empty, [], (0,0), (0,0))
    where
        chars' = Map.filter isUpper map'
        loop :: Map (Int, Int) Char -> (Map String (Int, Int), [((Int, Int), (Int, Int))], (Int, Int), (Int, Int)) -> ([((Int, Int), (Int, Int))], (Int, Int), (Int, Int))
        loop chars acc@(sp, portals, start, end) = let
            cMin = lookupMin chars
            Just (pos, c1) = cMin
            [neighPos] = Prelude.filter (\x -> member x chars) $ snd $ nextNeighbours Set.empty pos
            (name, pPoint) = getPortal map' pos neighPos
            newChars = delete neighPos $ delete pos chars
            pPoint2 = sp ! name
            in case cMin of
                Nothing -> (portals, start, end)
                _ -> case name of 
                    "AA" -> loop newChars (sp, portals, pPoint, end)
                    "ZZ" -> loop newChars (sp, portals, start, pPoint)
                    _ -> if member name sp
                        then loop newChars (delete name sp, (pPoint, pPoint2):(pPoint2, pPoint):portals, start, end)
                        else loop newChars (insert name pPoint sp, portals, start, end)

main = do
    -- x:_ <- getArgs
    v <- fromFile "in.txt"
    let a@(portals, start, end) = getPortals v
    let d = bfs a v
    print d

