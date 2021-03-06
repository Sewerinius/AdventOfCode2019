module B where

    import Data.Char (chr)
    import Data.List.Index (indexed)
    import Data.List.Split (splitOn)
    import Data.Map (Map, fromAscList, (!), insert, (!?), empty, filterWithKey, member, toList)
    import Data.Maybe (fromMaybe)
    import System.Environment
    import Debug.Trace (trace)

    run :: ((Map Int Int, Int, Int), [Int], [Int]) -> ((Map Int Int, Int, Int), [Int], [Int])
    -- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
    run res@((_, -1, _), _, _) = res
    run (intComp, inp, out) = run $ step intComp inp out

    step :: (Map Int Int, Int, Int) -> [Int] -> [Int] -> ((Map Int Int, Int, Int), [Int], [Int])
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
                1 -> (addInt mode (ip+1) rb v, inp, out)
                2 -> (multInt mode (ip+1) rb v, inp, out)
                3 -> readInt mode (ip+1) rb v inp out
                4 -> writeInt mode (ip+1) rb v inp out
                5 -> (jumpTrue mode (ip+1) rb v, inp, out)
                6 -> (jumpFalse mode (ip+1) rb v, inp, out)
                7 -> (lessThan mode (ip+1) rb v, inp, out)
                8 -> (equals mode (ip+1) rb v, inp, out)
                9 -> (adjustRelativeBase mode (ip+1) rb v, inp, out)
                99 -> ((v, -1, rb), inp, out)
                _ -> error $ "Unknown opcode " ++ show opcode ++ " at " ++ show ip

    adjustRelativeBase :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    adjustRelativeBase mode ip rb v =
        let
            [am] = digits 1 mode
            va = get v ip am rb
        in (v, ip+1, rb + va)

    equals :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    equals mode ip rb v =
        let
            [cm, bm, am] = digits 3 mode
            va = get v ip am rb
            vb = get v (ip+1) bm rb
            pc = address v (ip+2) cm rb
        in (insert pc (if va == vb then 1 else 0) v, ip+3, rb)

    lessThan :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    lessThan mode ip rb v =
        let
            [cm, bm, am] = digits 3 mode
            va = get v ip am rb
            vb = get v (ip+1) bm rb
            pc = address v (ip+2) cm rb
        in (insert pc (if va < vb then 1 else 0) v, ip+3, rb)

    jumpFalse :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    jumpFalse mode ip rb v =
        let
            [bm, am] = digits 2 mode
            va = get v ip am rb
            vb = get v (ip+1) bm rb
        in (v, if va == 0 then vb else ip+2, rb)

    jumpTrue :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    jumpTrue mode ip rb v =
        let
            [bm, am] = digits 2 mode
            va = get v ip am rb
            vb = get v (ip+1) bm rb
        in (v, if va /= 0 then vb else ip+2, rb)

    writeInt :: Int -> Int -> Int -> Map Int Int -> [Int] -> [Int] -> ((Map Int Int, Int, Int), [Int], [Int])
    writeInt mode ip rb v inp out =
        let
            [am] = digits 1 mode
            va = get v ip am rb
        in ((v, ip+1, rb), inp, va:out)
        -- in do 
            -- putStr [chr va]
            -- return ((v, ip+1, rb), inp, va:out)

    readInt :: Int -> Int -> Int -> Map Int Int -> [Int] -> [Int] -> ((Map Int Int, Int, Int), [Int], [Int])
    readInt mode ip rb v (x:inp) out =
        let
            [am] = digits 1 mode
            pa = address v ip am rb
        in ((insert pa x v, ip+1, rb), inp, out)

    multInt :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
    multInt mode ip rb v =
        let
            [cm, bm, am] = digits 3 mode
            va = get v ip am rb
            vb = get v (ip+1) bm rb
            pc = address v (ip+2) cm rb
        in (insert pc (va*vb) v, ip + 3, rb)

    addInt :: Int -> Int -> Int -> Map Int Int -> (Map Int Int, Int, Int)
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

    address :: Map Int Int -> Int -> Int -> Int -> Int
    -- address v ip mode rb
    address v ip 0 _ = v ! ip
    address v ip 1 _ = ip
    address v ip 2 rb = rb + v ! ip

    get :: Map Int Int -> Int -> Int -> Int -> Int
    get v ip mode rb = fromMaybe 0 (v !? address v ip mode rb)

    fromFile x = do
        s <- readFile x
        return $ map (read::String->Int) $ splitOn "," s

    checkM :: (Map Int Int, Int, Int) -> Int -> Int -> Bool
    checkM state x y = let
        (_,_,[res]) = run (state, [x, y], [])
        in res == 1

    run2 :: (Int -> Int -> Bool) -> Int -> (Int, Int)
    run2 check n = loop 0 0
        where
            loop x y | not $ check (x+n-1) y = loop x (y+1)
                     | not $ check x (y+n-1) = loop (x+1) y
                     | otherwise = (x, y)

    main = do
        -- x:_ <- getArgs
        v <- fromFile "in.txt"
        let sa = fromAscList $ indexed v
        let (x, y) = run2 (checkM (sa, 0, 0)) 100
        print (x,y)
        print $ x*10000 + y
