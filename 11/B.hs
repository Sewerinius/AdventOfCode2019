module B where

    import Control.Concurrent.Thread (forkIO)
    import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
    import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
    import Data.List (permutations)
    import Data.List.Index (indexed)
    import Data.List.Split (splitOn)
    import Data.Map (Map, empty, fromAscList, (!), insert, (!?), size, keys)
    import Data.Maybe (fromMaybe)
    -- import Data.Vector (Vector, fromList, (//), (!))
    import System.Environment
    import Debug.Trace (trace)

    run :: ((Data.Map.Map Int Int, Int, Int), Chan Int, Chan Int) -> IO (Data.Map.Map Int Int, Int, Int)
    -- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
    run (intComp, inp, out) = loop intComp
        where 
            loop (v, -1, rb) = return (v, -1, rb)
            loop intComp = loop =<< step intComp inp out

    step :: (Data.Map.Map Int Int, Int, Int) -> Chan Int -> Chan Int -> IO (Data.Map.Map Int Int, Int, Int)
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
                3 -> readInt mode (ip+1) rb v inp
                4 -> writeInt mode (ip+1) rb v out
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

    writeInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> Chan Int -> IO (Data.Map.Map Int Int, Int, Int)
    writeInt mode ip rb v out =
        let
            [am] = digits 1 mode
            va = get v ip am rb
        in do
            writeChan out va
            return (v, ip+1, rb)

    readInt :: Int -> Int -> Int -> Data.Map.Map Int Int -> Chan Int -> IO (Data.Map.Map Int Int, Int, Int)
    readInt mode ip rb v inp =
        let 
            [am] = digits 1 mode
            pa = address v ip am rb
        in do
            x <- readChan inp
            return (insert pa x v, ip+1, rb)


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

    data Direction = N | S | E | W

    move :: (Int, Int) -> Direction -> (Int, Int)
    move (x, y) N = (x, y+1)
    move (x, y) S = (x, y-1)
    move (x, y) E = (x-1, y)
    move (x, y) W = (x+1, y)

    rotate :: Direction -> Bool -> Direction
    rotate N True = W
    rotate S True = E
    rotate E True = N
    rotate W True = S
    rotate N False = E
    rotate S False = W
    rotate E False = S
    rotate W False = N

    robot :: Chan Int -> Chan Int -> MVar (Map (Int, Int) Bool) -> IO ()
    robot inp out mMap = loop (0, 0) N
        where
            loop pos@(x, y) dir = do 
                map <- takeMVar mMap
                let isPainted = fromMaybe False (map !? pos)

                -- putStrLn $ showTable map

                putMVar mMap map
                writeChan out $ if isPainted then 1 else 0
                color <- readChan inp
                rotation <- readChan inp
                map2 <- takeMVar mMap
                putMVar mMap $ insert pos (color /= 0) map2
                let newDir = rotate dir (rotation /= 0)
                loop (move pos newDir) newDir

    fromFile x = do
        s <- readFile x
        return $ Prelude.map (read::String->Int) $ splitOn "," s

    main = do
        -- x:_ <- getArgs
        v <- fromFile "in.txt"
        let sa = fromAscList $ indexed v
        toRobot <- newChan
        fromRobot <- newChan
        mMap <- newMVar $ fromAscList [((0, 0), True)]
        (_, res) <- forkIO $ run ((sa, 0, 0), fromRobot, toRobot)
        forkIO $ robot toRobot fromRobot mMap

        _ <- res
        map <- takeMVar mMap
        putStrLn $ showTable map

    showTable arr = 
        unlines $ map (map (\x -> if fromMaybe False (arr !? x) then '#' else ' ')) indices
        where 
            indices = [[(x, y) | x <- [startX..endX+1]] | y <- reverse [startY..endY+1]]
            ((startX, startY), (endX, endY)) = bounds arr

    bounds :: Map (Int, Int) a -> ((Int, Int), (Int, Int))
    bounds map = loop (keys map) ((0, 0), (0, 0))
        where 
            loop :: [(Int, Int)] -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
            loop [] acc = acc
            loop ((ax, ay):xs) ((sx, sy), (ex, ey)) = loop xs ((min ax sx, min ay sy), (max ax ex, max ay sy))
