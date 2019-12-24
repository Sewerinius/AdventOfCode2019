{-# LANGUAGE TupleSections #-}
module B where

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

type XY = (Int, Int)
type Point = (XY, Int)
type Visited = Set.Set Point
type Portal = (String, XY, XY, Int)
type EndsData = ([Portal], Point, Point)
type Labirynth = Map XY Char

nextNeighbours :: Visited -> Point -> (Visited, [Point])
nextNeighbours visited ((x, y), z) = let
    neighbours = map (, z) [(x, y+1),(x, y-1),(x-1, y),(x+1, y)]
    notVisited = Prelude.filter (`Set.notMember` visited) neighbours
    newVisited = foldl (flip Set.insert) visited notVisited
    in (newVisited, notVisited)

--TODO: Its may find faster routes going through the negetives

checkNeighbour :: Labirynth -> EndsData -> Point -> Point -> Maybe Point
checkNeighbour map' (portals, start@(startxy,_), end@(endxy,_)) mPos@(myPosxy, z) nPos@(neighPosxy, _) | up && myPosxy == startxy = Nothing
                                                         | up && mPos == end = Just nPos
                                                         | up && myPosxy == endxy = Nothing
                                                         | up && (z+dz) >= 0 = Just (pOut, z+dz)
                                                         | up = Nothing 
                                                         | c == '#' = Nothing
                                                         | otherwise = Just nPos
    where
        c = get map' neighPosxy
        up = isUpper c
        [(_,_,pOut, dz)] = Prelude.filter (\(_,pPos, _, _) -> pPos == myPosxy) portals

bfs :: EndsData -> Labirynth -> Int
bfs a@(portals, start, end) map' = loop newVisitedPoss iQueue
    where
        visitedPoss = Set.singleton start
        (newVisitedPoss, nextPoss) = nextNeighbours visitedPoss start
        iQueue = fromList $ map (, 1) $ mapMaybe (checkNeighbour map' a start) nextPoss
        loop :: Visited -> Seq (Point, Int) -> Int
        loop visited queue = let
            (pos@(posxy, z), dist) :< rest = viewl queue
            out = get map' posxy
            (newVisited, nextPoss) = nextNeighbours visited pos
            newComps = map (, dist+1) $ mapMaybe (checkNeighbour map' a pos) nextPoss
            newQueue = rest >< fromList newComps
            -- trac = trace ("bfsTrac -> pos=" ++ show pos ++ ", dist=" ++ show dist ++ ", out=" ++ show out ++ ", queue=" ++ show rest)
            trac = trace ("bfsTrac -> pos=" ++ show pos ++ ", dist=" ++ show dist ++ ", out=" ++ show out)
            in 
                -- if trac pos == end
                if pos == end
                then dist
                else
                    -- case trac out of
                    case out of
                    '#' -> loop visited rest
                    '.' -> loop newVisited newQueue
                    _ -> error "bfs shouldn't get here"
                            -- case map snd $ Prelude.filter (\(pPos, _) -> pPos == pos) portals of 
                            -- [] -> loop visited rest
                            -- [pOut] -> loop (Set.insert pOut visited) ((pOut, dist) <| rest)

get :: Ord a => Map a Char -> a -> Char
get m k = fromMaybe '#' $ m !? k

fromLine :: Int -> Int -> String -> Labirynth -> Labirynth
fromLine _ _ [] acc = acc
fromLine y x (' ':ss) acc = fromLine y (x+1) ss acc
fromLine y x (c:ss) acc = fromLine y (x+1) ss $ insert (x,y) c acc

fromLines :: Int -> [String] -> Labirynth -> Labirynth
fromLines _ [] acc = acc
fromLines y (line:lines) acc = fromLines (y+1) lines $ fromLine y 0 line acc

fromFile x = do
    s <- readFile x
    return $ fromLines 0 (lines s) empty

getPortal :: Labirynth -> XY -> XY -> (String, XY)
getPortal map' p1@(x1, y1) p2@(x2, y2) | p1 > p2 = getPortal map' p2 p1
                                        | x1 == x2 = let
                                        [endpoint] = Prelude.filter (\pos -> (=='.') $ fromMaybe '#' $ map' !? pos) [(x1, y1-1), (x2, y2+1)]
                                        name = [map' ! p1, map' ! p2]
                                        in (name, endpoint)
                                        | y1 == y2 = let
                                        [endpoint] = Prelude.filter (\pos -> (=='.') $ fromMaybe '#' $ map' !? pos) [(x1-1, y1), (x2+1, y2)]
                                        name = [map' ! p1, map' ! p2]
                                        in (name, endpoint)

getPortals :: Labirynth -> EndsData
getPortals map' = loop chars' (Map.empty, ([], ((0,0),0), ((0,0),0)))
    where
        chars' = Map.filter isUpper map'
        width = 116
        height = 124
        -- width = 45
        -- height = 36
        isOuter (x, y) = fromIntegral x < width * 0.15 || fromIntegral x > width * 0.85 || fromIntegral y < height * 0.15 || fromIntegral y > height * 0.85
        loop :: Labirynth -> (Map String (Int, Int), EndsData) -> EndsData
        loop chars acc@(sp, (portals, start, end)) = let
            cMin = lookupMin chars
            Just (pos, c1) = cMin
            [neighPos] = Prelude.filter (\x -> member x chars) $ map fst $ snd $ nextNeighbours Set.empty (pos, 0)
            (name, pPoint) = getPortal map' pos neighPos
            newChars = delete neighPos $ delete pos chars
            pPoint2 = sp ! name
            level = if isOuter pPoint then -1 else 1
            in case cMin of
                Nothing -> (portals, start, end)
                _ -> case name of
                    "AA" -> loop newChars (sp, (portals, (pPoint,0), end))
                    "ZZ" -> loop newChars (sp, (portals, start, (pPoint,0)))
                    _ -> if member name sp
                        then loop newChars (delete name sp, ((name, pPoint, pPoint2, level):(name, pPoint2, pPoint, -level):portals, start, end))
                        else loop newChars (insert name pPoint sp, (portals, start, end))

main = do
    -- x:_ <- getArgs
    v <- fromFile "in.txt"
    let a@(portals, start, end) = getPortals v
    let d = bfs a v
    print d

