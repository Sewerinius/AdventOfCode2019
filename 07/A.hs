module A where

    import Data.List (permutations)
    import Data.List.Split (splitOn)
    import Data.Vector (Vector, fromList, (//), (!))
    import System.Environment
    import Debug.Trace (trace)
    
    rate :: Data.Vector.Vector Int -> [Int] -> Int
    rate v [a, b, c, d, e] = do
        let (_, _, _, out1:_) = run (v, 0, [a, 0], []) 
        let (_, _, _, out2:_) = run (v, 0, [b, out1], []) 
        let (_, _, _, out3:_) = run (v, 0, [c, out2], []) 
        let (_, _, _, out4:_) = run (v, 0, [d, out3], []) 
        let (_, _, _, out5:_) = run (v, 0, [e, out4], []) 
        out5

    
    run :: (Data.Vector.Vector Int, Int, [Int], [Int]) -> (Data.Vector.Vector Int, Int, [Int], [Int])
    -- run (v, i) | trace ("run " ++ show v ++ " " ++ show i) False = undefined 
    run (v, -1, inp, out) = (v, -1, inp, out)
    run (v, i, inp, out) = run $ step v i inp out
    
    step :: Data.Vector.Vector Int -> Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    -- step v ip | trace ("step " ++ show v ++ " " ++ show ip) False = undefined 
    step v ip inp out =
        let
            opcode = v ! ip
            op = mod opcode 100
            mode = div opcode 100
            trac = trace ("stepTrac -> op=" ++ show opcode ++ ", mode=" ++ show mode ++ ", ip=" ++ show ip) 0
        in
            -- case trac + op of
            case op of
                1 -> addInt mode (ip+1) v inp out
                2 -> multInt mode (ip+1) v inp out
                3 -> readInt mode (ip+1) v inp out
                4 -> writeInt mode (ip+1) v inp out
                5 -> jumpTrue mode (ip+1) v inp out
                6 -> jumpFalse mode (ip+1) v inp out
                7 -> lessThan mode (ip+1) v inp out
                8 -> equals mode (ip+1) v inp out
                99 -> (v, -1, inp, out)
                _ -> error $ "Unknown opcode " ++ show opcode ++ " at " ++ show ip

    equals :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    equals mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
            pc = get v (ip+2) 1
        in if va == vb then (v // [(pc, 1)], ip+3, inp, out) else (v // [(pc, 0)], ip+3, inp, out)

    lessThan :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    lessThan mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
            pc = get v (ip+2) 1
        in if va < vb then (v // [(pc, 1)], ip+3, inp, out) else (v // [(pc, 0)], ip+3, inp, out)

    jumpFalse :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    jumpFalse mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
        in if va == 0 then (v, vb, inp, out) else (v, ip+2, inp, out)

    jumpTrue :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    jumpTrue mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
        in if va /= 0 then (v, vb, inp, out) else (v, ip+2, inp, out)
    
    writeInt :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    writeInt mode ip v inp out =
        let
            [am] = digits 1 mode
            va = get v ip am
        in (v, ip+1, inp, (va:out))
    -- writeInt mode ip v =
    --     let
    --         [am] = digits 1 mode
    --         va = get v ip am
    --     in do
    --         print va
    --         return (v, ip+1)
    
    readInt :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    readInt mode ip v (x:inp) out = 
        let pa = get v ip 1
        in (v // [(pa, x)], ip+1, inp, out)
    -- readInt mode ip v [] out =
    --     let pa = get v ip 1
    --     in do
    --         putStr "> "
    --         input <- readLn :: IO Int
    --         return (v // [(pa, input)], ip+1, [], out)
    
    
    multInt :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    multInt mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
            pc = get v (ip+2) 1
        in (v // [(pc, va * vb)], ip + 3, inp, out)
    
    addInt :: Int -> Int -> Data.Vector.Vector Int -> [Int] -> [Int] -> (Data.Vector.Vector Int, Int, [Int], [Int])
    addInt mode ip v inp out =
        let
            [bm, am] = digits 2 mode
            va = get v ip am
            vb = get v (ip+1) bm
            pc = get v (ip+2) 1
        in (v // [(pc, va + vb)], ip + 3, inp, out)
    
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
        print $ maximum [rate sa perm | perm <- permutations [0..4]]
    
    fromFile x = do
        s <- readFile x
        let sa = Prelude.map (read::String->Int) $ splitOn "," s
        prog sa
    
    main = do
        x:_ <- getArgs
        fromFile x
    
    