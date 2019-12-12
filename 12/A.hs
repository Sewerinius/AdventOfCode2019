module A where

    import Debug.Trace (trace)

    data Asteroid = Asteroid {pos :: (Int, Int, Int), vel :: (Int, Int, Int) } deriving (Show)

    newAsteroid :: Int -> Int -> Int -> Asteroid
    newAsteroid x y z = Asteroid {pos = (x, y, z), vel = (0, 0, 0)}

    sign :: Int -> Int
    sign x | x < 0 = -1
           | x == 0 = 0
           | otherwise = 1

    updateVelocity :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    updateVelocity (ax, ay, az) (bx, by, bz) (vx, vy, vz) = (vx + sign (bx - ax), vy + sign (by - ay), vz + sign (bz - az))

    updateAsteroid :: Asteroid -> [Asteroid] -> Asteroid
    updateAsteroid Asteroid{pos=p@(x, y, z), vel=v} as = 
        let
            nv@(nx, ny, nz) = foldl (\b Asteroid{pos=p2} -> updateVelocity p p2 b) v as
        in Asteroid{pos=(x+nx, y+ny, z+nz), vel=nv}

    step :: [Asteroid] -> [Asteroid]
    step as = map (`updateAsteroid` as) as

    loop :: Int -> [Asteroid] -> [Asteroid]
    loop 0 as = as
    loop n as = loop (n-1) $ step as

    getEnergy :: Asteroid -> Int
    getEnergy Asteroid{pos=(x, y, z), vel=(vx, vy, vz)} = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz) 

    getEnergies :: [Asteroid] -> Int
    getEnergies = foldl (\s a -> s + getEnergy a) 0

    main = do
        -- x:_ <- getArgs
        let asteroids = [newAsteroid (-10) (-13) 7, newAsteroid 1 2 1, newAsteroid (-15) (-3) 13, newAsteroid 3 7 (-4)]
        -- let asteroids = [newAsteroid (-1) 0 2, newAsteroid 2 (-10) (-7), newAsteroid 4 (-8) 8, newAsteroid 3 5 (-1)]
        print $ getEnergies $ loop 1000 asteroids


