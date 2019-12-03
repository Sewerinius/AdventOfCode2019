module A where

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
    
    prog x = do 
        let sa = fromList x
        print $ readOp (sa // [(1, 12), (2, 2)], 0)
    
    fromFile x = do
        s <- readFile x
        let sa = Prelude.map (read::String->Int) $ splitOn "," s
        prog sa
    
    main = do
        x:_ <- getArgs
        fromFile x
        
    