module A where

import Data.List.Index (indexed)
import Data.List.Split (splitOn)
import Data.Map (Map, fromAscList, (!), insert, (!?))
import qualified Data.Set as Set (Set, notMember, insert, singleton)
import Data.Sequence (Seq, viewl, ViewL(..), fromList, (><))
import Data.Maybe (fromMaybe)
import System.Environment
import Debug.Trace (trace)

nextNeighbours :: Set.Set (Int, Int) -> (Int, Int) -> (Set.Set (Int, Int), [((Int, Int), Int)])
nextNeighbours visited (x, y) = let
    neighbours = [((x, y+1), 1),((x, y-1), 2),((x-1, y), 3),((x+1, y), 4)]
    notVisited = filter (\(pos, _) -> Set.notMember pos visited) neighbours
    newVisited = foldl (flip Set.insert) visited $ map fst notVisited
    in (newVisited, notVisited)

bfs :: Set.Set (Int, Int) -> Seq ((Int, Int), Int, ((Data.Map.Map Int Int, Int, Int), [Int], [Int])) -> Int
bfs visited queue = let 
    (pos, val, comp) :< rest = viewl queue
    (state, _, out:_) = run (comp, False)
    (newVisited, newData) = nextNeighbours visited pos 
    newComps = map (\(pos, dir) -> (pos, val+1, (state, [dir], []))) newData
    newQueue = rest >< fromList newComps
    trac = trace ("bfsTrac -> pos=" ++ show pos ++ ", val=" ++ show val ++ ", out=" ++ show out) 0
    -- in case trac + out of 
    in case out of 
        2 -> val
        1 -> bfs newVisited newQueue
        0 -> bfs visited rest

run :: (((Data.Map.Map Int Int, Int, Int), [Int], [Int]), Bool) -> ((Data.Map.Map Int Int, Int, Int), [Int], [Int])
-- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
run (res, True) = res
run (res@((_, -1, _), _, _), False) = res
run ((intComp, inp, out), False) = run $ step intComp inp out

step :: (Data.Map.Map Int Int, Int, Int) -> [Int] -> [Int] -> (((Data.Map.Map Int Int, Int, Int), [Int], [Int]), Bool)
-- step v ip | trace ("step " ++ show v ++ " " ++ show ip) False = undefined 
step (v, ip, rb) inp out =
    let
        opcode = v ! ip
        op = mod opcode 100
        mode = div opcode 100
        trac = trace ("stepTrac -> op=" ++ show opcode ++ ", mode=" ++ show mode ++ ", ip=" ++ show ip) 0 -- ++ ", v=" ++ show v) 0
    in
        -- case trac + op of
        case op of
            1 -> ((addInt mode (ip+1) rb v, inp, out), False)
            2 -> ((multInt mode (ip+1) rb v, inp, out), False)
            3 -> readInt mode (ip+1) rb v inp out
            4 -> (writeInt mode (ip+1) rb v inp out, False)
            5 -> ((jumpTrue mode (ip+1) rb v, inp, out), False)
            6 -> ((jumpFalse mode (ip+1) rb v, inp, out), False)
            7 -> ((lessThan mode (ip+1) rb v, inp, out), False)
            8 -> ((equals mode (ip+1) rb v, inp, out), False)
            9 -> ((adjustRelativeBase mode (ip+1) rb v, inp, out), False)
            99 -> (((v, -1, rb), inp, out), False)
            _ -> error $ "Unknown opcode " ++ show opcode ++ " at " ++ show ip

adjustRelativeBase :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
adjustRelativeBase mode ip rb v =
    let
        [am] = digits 1 mode
        va = get v ip am rb
    in (v, ip+1, rb + va)

equals :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
equals mode ip rb v =
    let
        [cm, bm, am] = digits 3 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
        pc = address v (ip+2) cm rb
    in (insert pc (if va == vb then 1 else 0) v, ip+3, rb)

lessThan :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
lessThan mode ip rb v =
    let
        [cm, bm, am] = digits 3 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
        pc = address v (ip+2) cm rb
    in (insert pc (if va < vb then 1 else 0) v, ip+3, rb)

jumpFalse :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
jumpFalse mode ip rb v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
    in (v, if va == 0 then vb else ip+2, rb)

jumpTrue :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
jumpTrue mode ip rb v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
    in (v, if va /= 0 then vb else ip+2, rb)

writeInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> [Int] -> [Int] -> ((Data.Map.Map Int Int, Int, Int), [Int], [Int])
writeInt mode ip rb v inp out =
    let
        [am] = digits 1 mode
        va = get v ip am rb
    in ((v, ip+1, rb), inp, va:out)

readInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> [Int] -> [Int] -> (((Data.Map.Map Int Int, Int, Int), [Int], [Int]), Bool)
readInt mode ip rb v [] out = (((v, ip-1, rb), [], out), True)
readInt mode ip rb v (x:inp) out =
    let 
        [am] = digits 1 mode
        pa = address v ip am rb
    in (((insert pa x v, ip+1, rb), inp, out), False)

multInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
multInt mode ip rb v =
    let
        [cm, bm, am] = digits 3 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
        pc = address v (ip+2) cm rb
    in (insert pc (va*vb) v, ip + 3, rb)

addInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> (Data.Map.Map Int Int, Int, Int)
addInt mode ip rb v =
    let
        [cm, bm, am] = digits 3 mode
        va = get v ip am rb
        vb = get v (ip+1) bm rb
        pc = address v (ip+2) cm rb
    in (insert pc (va+vb) v, ip + 3, rb)

digits :: Int -> Int -> [Int]
digits n x = loop n x []
    where
        loop 0 _ acc = acc
        loop n x acc = loop (n-1) (x `div` 10) ((x`mod`10):acc)

address :: Data.Map.Map Int Int -> Int -> Int -> Int -> Int
-- address v ip mode rb
address v ip 0 _ = v ! ip
address v ip 1 _ = ip
address v ip 2 rb = rb + v ! ip

get :: Data.Map.Map Int Int -> Int -> Int -> Int -> Int
get v ip mode rb = fromMaybe 0 (v !? address v ip mode rb)

prog x = do
    let sa = fromAscList $ indexed x
    let visited = Set.singleton (0, 0)
    let (newVisited, newData) = nextNeighbours visited (0, 0) 
    let queue = fromList $ map (\(pos, dir) -> (pos, 1, ((sa, 0, 0), [dir], []))) newData
    let out = bfs newVisited queue
    print out

fromFile x = do
    s <- readFile x
    return $ Prelude.map (read::String->Int) $ splitOn "," s

main = do
    -- x:_ <- getArgs
    v <- fromFile "in.txt"
    prog v

