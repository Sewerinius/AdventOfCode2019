module B where

    import Data.List.Split (splitOn)
    import Data.Vector (Vector, fromList, (//), (!))
    import System.Environment
    import Debug.Trace (trace)
    
    
    run :: (Data.Vector.Vector Int, Int) -> IO (Data.Vector.Vector Int, Int)
    -- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
    run (v, -1) = return (v, -1)
    run (v, i) = run =<< step v i
    
    step :: Data.Vector.Vector Int -> Int -> IO (Data.Vector.Vector Int, Int)
    -- step v ip | trace ("step " ++ show v ++ " " ++ show ip) False = undefined 
    step v ip =
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
                3 -> readInt mode (ip+1) v
                4 -> writeInt mode (ip+1) v
                5 -> return $ jumpTrue mode (ip+1) v
                6 -> return $ jumpFalse mode (ip+1) v
                7 -> return $ lessThan mode (ip+1) v
                8 -> return $ equals mode (ip+1) v
                99 -> return (v, -1)
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
    
    writeInt :: Int -> Int -> Data.Vector.Vector Int -> IO (Data.Vector.Vector Int, Int)
    writeInt mode ip v =
        let
            [am] = digits 1 mode
            va = get v ip am
        in do
            print va
            return (v, ip+1)
    
    readInt :: Int -> Int -> Data.Vector.Vector Int -> IO (Data.Vector.Vector Int, Int)
    readInt mode ip v =
        let pa = get v ip 1
        in do
            putStr "> "
            input <- readLn :: IO Int
            return (v // [(pa, input)], ip+1)
    
    
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
        print =<< run (sa, 0)
    
    fromFile x = do
        s <- readFile x
        let sa = Prelude.map (read::String->Int) $ splitOn "," s
        prog sa
    
    main = do
        x:_ <- getArgs
        fromFile x
    
    