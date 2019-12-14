module A where

import Control.Concurrent.Thread (forkIO)
import Control.Concurrent.STM.TMChan (TMChan, newTMChan, writeTMChan, readTMChan, closeTMChan, isEmptyTMChan)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, takeTMVar, putTMVar, isEmptyTMVar, readTMVar)
import GHC.Conc (STM, atomically)
import Data.List (permutations)
import Data.List.Index (indexed)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, fromAscList, (!), insert, (!?), size, keys)
import Data.Maybe (fromMaybe)
import System.Environment
import Debug.Trace (trace)
import Util (count)
import Data.Tuple.Extra (thd3)

run :: ((Data.Map.Map Int Int, Int, Int), TMChan Int, TMChan Int) -> IO (Data.Map.Map Int Int, Int, Int)
-- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
run (intComp, inp, out) = loop intComp
    where 
        loop (v, -1, rb) = do 
            atomically $ closeTMChan out
            return (v, -1, rb)
        loop intComp = loop =<< step intComp inp out

step :: (Data.Map.Map Int Int, Int, Int) -> TMChan Int -> TMChan Int -> IO (Data.Map.Map Int Int, Int, Int)
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
            1 -> return $ addInt mode (ip+1) rb v
            2 -> return $ multInt mode (ip+1) rb v
            3 -> atomically $ readInt mode (ip+1) rb v inp
            4 -> atomically $ writeInt mode (ip+1) rb v out
            5 -> return $ jumpTrue mode (ip+1) rb v
            6 -> return $ jumpFalse mode (ip+1) rb v
            7 -> return $ lessThan mode (ip+1) rb v
            8 -> return $ equals mode (ip+1) rb v
            9 -> return $ adjustRelativeBase mode (ip+1) rb v
            99 -> return (v, -1, rb)
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

writeInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> TMChan Int -> STM (Data.Map.Map Int Int, Int, Int)
writeInt mode ip rb v out =
    let
        [am] = digits 1 mode
        va = get v ip am rb
    in do
        writeTMChan out va
        return (v, ip+1, rb)

readInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> TMChan Int -> STM (Data.Map.Map Int Int, Int, Int)
readInt mode ip rb v inp =
    let 
        [am] = digits 1 mode
        pa = address v ip am rb
    in do
        x <- readTMChan inp
        case x of
            Nothing -> error "Chan is closed"
            Just y -> return (insert pa y v, ip+1, rb)


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

fromFile x = do
    s <- readFile x
    return $ Prelude.map (read::String->Int) $ splitOn "," s

getTMChanContents :: TMChan a -> STM [a]
getTMChanContents ch = do
    mx <- readTMChan ch
    case mx of 
        Nothing -> return []
        Just y -> do 
            ys <- getTMChanContents ch
            return (y:ys)

cnv :: [a] -> [((a, a), a)]
cnv [] = []
cnv (x:y:z:xs) = ((x, y), z):cnv xs

main = do
    v <- fromFile "in.txt"
    let sa = fromAscList $ indexed v
    out <- atomically newTMChan
    inp <- atomically newTMChan
    (_, res) <- forkIO $ run ((sa, 0, 0), inp, out)

    _ <- res
    contents <- atomically $ getTMChanContents out
    -- print $ count ((==2) . snd) $ cnv contents
    putStrLn $ showTable $ fromList $ cnv contents

showTile :: Maybe Int -> Char
showTile Nothing = ' '
showTile (Just 0) = ' '
showTile (Just 1) = '|'
showTile (Just 2) = '#'
showTile (Just 3) = '-'
showTile (Just 4) = 'o'

showTable arr = 
    unlines $ map (map (\x -> showTile (arr !? x))) indices
    where 
        indices = [[(x, y) | x <- [startX..endX+1]] | y <- [startY..endY+1]]
        ((startX, startY), (endX, endY)) = ((0,0),(40,24))
