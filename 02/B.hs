module B where

    import Data.List.Split (splitOn)
    import Data.Vector (fromList, (//), (!))
    import System.Environment
    import Debug.Trace (trace)
    
    -- readOp (v, i) | trace ("readOp " ++ show v ++ " " ++ show i) False = undefined 
    readOp (v, -1) = (v, -1)
    readOp (v, i) = readOp $ op (v ! i) v (i + 1)
    
    op 1 v i = (v // [(v ! (i+2), v `g` i + (v `g` (i + 1)))], (i + 3))
    op 2 v i = (v // [(v ! (i+2), (v `g` i) * (v `g` (i + 1)))], (i + 3))
    op 99 v _ = (v, -1)
    op opcode _ i = error $ "Unknown Op " ++ show opcode ++ " at " ++ show (i-1)
    
    g = get
    
    get v i = v ! (v ! i)
    
    prog x a b = do 
        let sa = fromList x
        readOp (sa // [(1, a), (2, b)], 0)
    
    fromFile x = do
        s <- readFile x
        let sa = Prelude.map (read::String->Int) $ splitOn "," s
        print $ filter (\(i, j, (v, k)) -> (v ! 0) == 19690720) [(i, j, prog sa i j) | i <- [0..99], j <- [0..99]]
    
    main = do
        x:_ <- getArgs
        fromFile x
        
    