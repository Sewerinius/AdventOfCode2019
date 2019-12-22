module A where

    import Data.Char (digitToInt)
    import System.Environment
    import Debug.Trace (trace)

    genPattern :: Int -> [Int]
    genPattern n = cycle $ concatMap (replicate n) [1, 0, -1, 0] 

    lastDigit x = mod (abs x) 10

    run :: Int -> [Int] -> [Int]
    run 0 xs = xs
    run n xs = run (n-1) $ step xs

    step :: [Int] -> [Int]
    step = loop 1
        where
            loop _ [] = []
            loop n l@(_:xs) = (lastDigit $ sum $ zipWith (*) l (genPattern n)) : loop (n+1) xs

    fromFile x = do
        s <- readFile x
        return $ map digitToInt s

    main = do
        -- x:_ <- getArgs
        v <- fromFile "in.txt"
        putStrLn $ concatMap show $ take 8 $ run 100 v

