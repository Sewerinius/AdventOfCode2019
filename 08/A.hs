module A where

    import Data.List.Split (chunksOf)
    import Data.List.Utils (countElem)
    import System.Environment
    import Debug.Trace (trace)
    import Util (minWith)

    width = 25
    height = 6

    prog x = do
        let sa = chunksOf (width * height) x
        let minLayer = snd $ minWith fst $ map (\x -> (countElem '0' x, x)) sa
        print $ (countElem '1' minLayer) * (countElem '2' minLayer)

    fromFile x = do
        s <- readFile x
        prog s

    main = do
        x:_ <- getArgs
        fromFile x

