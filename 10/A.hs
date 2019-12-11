module A where

    import Data.Set (Set, empty, fromList, size)
    import Data.List.Extra (maximumOn)
    import System.Environment (getArgs)
    import Debug.Trace (trace)

    data Asteroid = Asteroid {posX::Int, posY::Int} deriving (Show, Eq)

    getDirection :: Asteroid -> Asteroid -> (Int, Int)
    getDirection a b = 
        let 
            dx = posX b - posX a
            dy = posY b - posY a
            gcd' = gcd dx dy
        in (div dx gcd', div dy gcd')

    getDirections :: Asteroid -> [Asteroid] -> Set (Int, Int)
    getDirections a bs = fromList $ map (getDirection a) $ filter (/=a) bs

    fromLine :: Int -> Int -> String -> [Asteroid] -> [Asteroid]
    fromLine _ _ [] acc = acc
    fromLine y x ('.':ss) acc = fromLine y (x+1) ss acc
    fromLine y x ('#':ss) acc = fromLine y (x+1) ss ((Asteroid {posX = x, posY = y}):acc) 

    fromLines :: Int -> [String] -> [Asteroid] -> [Asteroid]
    fromLines _ [] acc = acc
    fromLines y (line:lines) acc = fromLines (y+1) lines $ fromLine y 0 line acc

    fromFile x = do
        s <- readFile x
        return $ fromLines 0 (lines s) []

    main = do
        -- x:_ <- getArgs
        asteroids <- fromFile "in.txt"
        print $ (size . flip getDirections asteroids) $ maximumOn (size . flip getDirections asteroids) asteroids 

