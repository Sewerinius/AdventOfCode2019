module B2 where

    import Data.Char (digitToInt)
    import System.Environment
    import Debug.Trace (trace)

    genPattern :: Int -> [Int]
    genPattern n = cycle $ concatMap (replicate n) [1, 0, -1, 0] 

    lastDigit x = mod (abs x) 10

    run :: Int -> [Int] -> Int -> [Int]
    run n xs _ | trace ("run n = " ++ show n ++ show (take 7 xs)) False = undefined
    run 0 xs _ = xs
    run n xs skip = run (n-1) (step xs skip) skip

    step :: [Int] -> Int -> [Int]
    step xs skip = loop (skip+1) xs
        where
            loop _ [] = []
            loop n l@(_:xs) = (lastDigit $ sum $ zipWith (*) l (drop skip $ genPattern n)) : loop (n+1) xs

    fromFile x = do
        s <- readFile x
        return $ map digitToInt s

    toInt :: [Int] -> Int
    toInt = foldl (\a b -> a * 10 + b) 0

    main = do
        -- x:_ <- getArgs
        v <- fromFile "in.txt"
        let l = length v
        let skip = toInt $ take 7 v
        let v2 = drop skip $ take (10000*l) $ cycle v
        putStrLn $ concatMap show $ take 8 $ run 100 v2 skip

