{-# LANGUAGE TupleSections #-}
module B where
    -- TODO:
    -- slow
    -- change it to a graph with distances, so it stops checking them all the time

    import Data.Char (toUpper, isLower)
    import Data.List.Index (indexed)
    import Data.List.Split (splitOn)
    import Data.List.HT (splitEverywhere)
    import Data.Map as Map (Map, fromAscList, (!), insert, (!?), member, empty, filter, toList)
    import qualified Data.Set as Set -- (Set, member, notMember, insert, singleton, empty)
    import qualified Data.PSQueue as PSQ -- (PSQ)
    import Data.Sequence (Seq, viewl, ViewL(..), fromList, (><))
    import Data.Maybe (fromMaybe)
    import System.Environment
    import Debug.Trace (trace)

    nextNeighbours :: Set.Set (Int, Int) -> (Int, Int) -> (Set.Set (Int, Int), [(Int, Int)])
    nextNeighbours visited (x, y) = let
        neighbours = [(x, y+1),(x, y-1),(x-1, y),(x+1, y)]
        notVisited = Prelude.filter (\pos -> Set.notMember pos visited) neighbours
        newVisited = foldl (flip Set.insert) visited notVisited
        in (newVisited, notVisited)

    bfs :: Set.Set Char -> (Int, Int) -> Int -> Map (Int, Int) Char -> [((Int, Int), Set.Set Char, Char, Int)]
    bfs keys pos iDist map' = loop newVisitedPoss iQueue []
        where 
            visitedPoss = Set.singleton pos
            (newVisitedPoss, newData) = nextNeighbours visitedPoss pos
            iQueue = fromList $ map (, iDist+1) newData
            loop visited queue acc = let
                viewl' = viewl queue
                (pos, dist) :< rest = viewl'
                out = get map' pos
                -- (state, _, out:_) = run (comp, False)
                (newVisited, newData) = nextNeighbours visited pos
                newComps = map (, dist+1) newData
                newQueue = rest >< fromList newComps
                trac = trace ("bfsTrac -> pos=" ++ show pos ++ ", dist=" ++ show dist ++ ", out=" ++ show out) 0
                -- in case trac + out of 
                in case viewl' of
                    EmptyL -> acc
                    _ -> case out of
                        '#' -> loop visited rest acc
                        '.' -> loop newVisited newQueue acc
                        '@' -> loop newVisited newQueue acc
                        c -> if Set.member (toUpper c) keys 
                            then loop newVisited newQueue acc
                            else if isLower c
                                then loop visited rest ((pos, Set.insert (toUpper c) keys, c, dist):acc)
                                else loop visited rest acc

    get :: Ord a => Map a Char -> a -> Char
    get m k = fromMaybe '#' $ m !? k

    fromLine :: Int -> Int -> String -> Map (Int, Int) Char -> Map (Int, Int) Char
    fromLine _ _ [] acc = acc
    fromLine y x (c:ss) acc = fromLine y (x+1) ss $ insert (x,y) c acc

    fromLines :: Int -> [String] -> Map (Int, Int) Char -> Map (Int, Int) Char
    fromLines _ [] acc = acc
    fromLines y (line:lines) acc = fromLines (y+1) lines $ fromLine y 0 line acc

    fromFile x = do
        s <- readFile x
        return $ fromLines 0 (lines s) empty

    run :: Set.Set ([(Int, Int)], Set.Set Char) -> Map (Int, Int) Char -> PSQ.PSQ ([(Int, Int)], (Set.Set Char), [Char]) Int -> Int
    run _ _ pqueue | trace ("run top=" ++ (show $ PSQ.findMin pqueue)) False = undefined
    run visitedKeys map' pqueue = let
        Just (((poss, keys, travel) PSQ.:-> dist), pqueue2) = PSQ.minView pqueue
        outs = concatMap (\(h, pos, t) -> map (\(pos', keys', key, dist') -> ((h ++ [pos'] ++ t), keys', key, dist')) $ bfs keys pos dist map') $ splitEverywhere poss
        pqueue3 = foldl (\psq (poss, keys', key, dist') -> PSQ.insert (poss, keys', (key:travel)) dist' psq) pqueue2 outs
        newVisitedKeys = Set.insert (poss,keys) visitedKeys
        in 
            if Set.notMember (poss,keys) visitedKeys
            then case outs of 
                [] -> dist
                _ -> run newVisitedKeys map' pqueue3
            else run visitedKeys map' pqueue2

    main = do
        -- x:_ <- getArgs
        v <- fromFile "in2.txt"
        let sPoss = map fst $ toList $ Map.filter (=='@') v
        let out = run Set.empty v $ PSQ.singleton (sPoss, Set.empty, []) 0
        print out

