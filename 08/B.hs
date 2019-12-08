module B where

    import Data.List (transpose, intercalate)
    import Data.List.Split (chunksOf)
    import Data.List.Utils (countElem)
    import System.Environment
    import Debug.Trace (trace)
    import Util (minWith)

    width = 25
    height = 6

    mergeLayers :: [String] -> String
    mergeLayers l = loop (transpose l) []
        where 
            loop :: [String] -> String -> String
            loop [] acc = reverse acc
            loop (x:xs) acc = loop xs (head (dropWhile (=='2') x):acc)

    prog x = do
        let sa = chunksOf (width * height) x
        mapM print $ chunksOf width $ mergeLayers sa

    fromFile x = do
        s <- readFile x
        prog s

    main = do
        x:_ <- getArgs
        fromFile x

