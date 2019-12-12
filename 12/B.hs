module B where

    import Debug.Trace (trace)
    import Data.Map (Map, empty, insert, member, (!))

    data Asteroid = Asteroid {pos :: Int, vel :: Int} deriving (Show, Eq, Ord)

    newAsteroid :: Int -> Asteroid
    newAsteroid x = Asteroid {pos = x, vel = 0}

    sign :: Int -> Int
    sign x | x < 0 = -1
           | x == 0 = 0
           | otherwise = 1

    updateVelocity :: Int -> Int -> Int -> Int
    updateVelocity a b v = v + sign (b - a)

    updateAsteroid :: Asteroid -> [Asteroid] -> Asteroid
    updateAsteroid Asteroid{pos=p, vel=v} as = 
        let
            nv = foldl (\b Asteroid{pos=p2} -> updateVelocity p p2 b) v as
        in Asteroid{pos=p+nv, vel=nv}

    step :: [Asteroid] -> [Asteroid]
    step as = map (`updateAsteroid` as) as

    loop :: Int -> [Asteroid] -> Map [Asteroid] Int -> (Int, Int)
    loop n as m = 
        if member as m then (n, m ! as)
        else loop (n+1) (step as) $ insert as n m

    main = do
        -- let asteroidsX = [newAsteroid (-1), newAsteroid 2, newAsteroid 4, newAsteroid 3]
        -- let asteroidsY = [newAsteroid 0, newAsteroid (-10), newAsteroid (-8), newAsteroid 5]
        -- let asteroidsZ = [newAsteroid 2, newAsteroid (-7), newAsteroid 8, newAsteroid (-1)]
        let asteroidsX = [newAsteroid (-10), newAsteroid 1, newAsteroid (-15), newAsteroid 3]
        let asteroidsY = [newAsteroid (-13), newAsteroid 2, newAsteroid (-3), newAsteroid 7]
        let asteroidsZ = [newAsteroid 7, newAsteroid 1, newAsteroid 13, newAsteroid (-4)]

        let (a, _) = loop 0 asteroidsX empty
        let (b, _) = loop 0 asteroidsY empty
        let (c, _) = loop 0 asteroidsZ empty

        print $ lcm a $ lcm b c


