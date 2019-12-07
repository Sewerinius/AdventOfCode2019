module B where

import Control.Concurrent.Thread (forkIO, result)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan, getChanContents)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Vector (Vector, fromList, (//), (!))
import System.Environment
import Debug.Trace (trace)

rate :: Data.Vector.Vector Int -> [Int] -> IO Int
rate v [a, b, c, d, e] = do
    ab <- newChan
    bc <- newChan
    cd <- newChan
    de <- newChan
    ea <- newChan
    writeChan ea a 
    writeChan ea 0
    writeChan ab b
    writeChan bc c
    writeChan cd d
    writeChan de e
    forkIO $ run (v, 0, ea, ab, False) "a"
    forkIO $ run (v, 0, ab, bc, False) "b"
    forkIO $ run (v, 0, bc, cd, False) "c"
    forkIO $ run (v, 0, cd, de, False) "d"
    (_, res) <- forkIO $ run (v, 0, de, ea, True) "e" 
    res2 <- res
    (_, res3) <- result res2
    return res3

extract :: (Monad m) => (a, m b) -> m (a, b)
extract (a, b) = do
    c <- b
    return (a, c)


run :: (Data.Vector.Vector Int, Int, Chan Int, Chan Int, Bool) -> String -> IO (Data.Vector.Vector Int, Int)
-- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
-- run (v, -1, inChan, outChan) = return (v, -1, inp, out)
run (v, ip, inChan, outChan, ret) dbg = -- run $ step v ip inp out
    loop (v, ip)
    where
        loop (v, -1) = if ret then extract (v, readChan outChan) else return (v, -1)
        loop (v, ip) = loop =<< step v ip inChan outChan dbg

step :: Data.Vector.Vector Int -> Int -> Chan Int -> Chan Int -> String -> IO (Data.Vector.Vector Int, Int)
-- step v ip | trace ("step " ++ show v ++ " " ++ show ip) False = undefined 
step v ip inChan outChan dbg =
    let
        opcode = v ! ip
        op = mod opcode 100
        mode = div opcode 100
        trac = trace ("stepTrac -> op=" ++ show opcode ++ ", mode=" ++ show mode ++ ", ip=" ++ show ip) 0
    in
        -- case trac + op of
        case op of
            1 -> return $ addInt mode (ip+1) v
            2 -> return $ multInt mode (ip+1) v
            3 -> readInt mode (ip+1) v inChan
            4 -> writeInt mode (ip+1) v outChan dbg
            5 -> return $ jumpTrue mode (ip+1) v
            6 -> return $ jumpFalse mode (ip+1) v
            7 -> return $ lessThan mode (ip+1) v
            8 -> return $ equals mode (ip+1) v
            99 -> return $ (v, -1)
            _ -> error $ "Unknown opcode " ++ show opcode ++ " at " ++ show ip

equals :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
equals mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
        pc = get v (ip+2) 1
    in if va == vb then (v // [(pc, 1)], ip+3) else (v // [(pc, 0)], ip+3)

lessThan :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
lessThan mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
        pc = get v (ip+2) 1
    in if va < vb then (v // [(pc, 1)], ip+3) else (v // [(pc, 0)], ip+3)

jumpFalse :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
jumpFalse mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
    in if va == 0 then (v, vb) else (v, ip+2)

jumpTrue :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
jumpTrue mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
    in if va /= 0 then (v, vb) else (v, ip+2)

writeInt :: Int -> Int -> Data.Vector.Vector Int -> Chan Int -> String -> IO (Data.Vector.Vector Int, Int)
writeInt mode ip v outChan dbg =
    let
        [am] = digits 1 mode
        va = get v ip am
    in do
        writeChan outChan (va) -- + trace (dbg ++ " write value: " ++ show va) 0)
        return (v, ip+1)
-- writeInt mode ip v =
--     let
--         [am] = digits 1 mode
--         va = get v ip am
--     in do
--         print va
--         return (v, ip+1)

readInt :: Int -> Int -> Data.Vector.Vector Int -> Chan Int -> IO (Data.Vector.Vector Int, Int)
readInt mode ip v inChan = 
    let pa = get v ip 1
    in do 
        x <- readChan inChan
        return (v // [(pa, x)], ip+1)
-- readInt mode ip v [] out =
--     let pa = get v ip 1
--     in do
--         putStr "> "
--         input <- readLn :: IO Int
--         return (v // [(pa, input)], ip+1, [], out)


multInt :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
multInt mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
        pc = get v (ip+2) 1
    in (v // [(pc, va * vb)], ip + 3)

addInt :: Int -> Int -> Data.Vector.Vector Int -> (Data.Vector.Vector Int, Int)
addInt mode ip v =
    let
        [bm, am] = digits 2 mode
        va = get v ip am
        vb = get v (ip+1) bm
        pc = get v (ip+2) 1
    in (v // [(pc, va + vb)], ip + 3)

digits :: Int -> Int -> [Int]
digits n x = loop n x []
    where
        loop 0 _ acc = acc
        loop n x acc = loop (n-1) (x `div` 10) ((x`mod`10):acc)

get :: Data.Vector.Vector Int -> Int -> Int -> Int
get v i 0 = v ! (v ! i)
get v i 1 = v ! i

prog x = do
    let sa = fromList x
    sb <- sequence [rate sa perm | perm <- permutations [5..9]]
    print $ maximum sb

fromFile x = do
    s <- readFile x
    let sa = Prelude.map (read::String->Int) $ splitOn "," s
    prog sa

-- main = do
--     x:_ <- getArgs
--     fromFile x

