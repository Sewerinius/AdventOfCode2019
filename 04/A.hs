module A where

inc :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
inc (a, b, c, d, e, f) | f < 9 = (a, b, c, d, e, f+1)
                       | e < 9 = (a, b, c, d, e+1, e+1)
                       | d < 9 = (a, b, c, d+1, d+1, d+1)
                       | c < 9 = (a, b, c+1, c+1, c+1, c+1)
                       | b < 9 = (a, b+1, b+1, b+1, b+1, b+1)
                       | a < 9 = (a+1, a+1, a+1, a+1, a+1, a+1)
                       | otherwise = error "tooBig"

isCorrect :: (Int, Int, Int, Int, Int, Int) -> Bool
isCorrect a = allSmallerOrEqual a && equalPair a

allSmallerOrEqual :: (Int, Int, Int, Int, Int, Int) -> Bool
allSmallerOrEqual (a, b, c, d, e, f) = a <= b && b <= c && c <= d && d <= e && e <= f

equalPair :: (Int, Int, Int, Int, Int, Int) -> Bool
equalPair (a, b, c, d, e, f) = a == b || b == c || c == d || d == e || e == f

(<=.) :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> Bool
(a, b, c, d, e, f) <=. (a2, b2, c2, d2, e2, f2) = a < a2 || (a == a2 && (b < b2 || (b == b2 && (c < c2 || (c == c2 && (d < d2 || (d == d2 && (e < e2 || (e == e2 && (f <= f2))))))))))

count :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> Int
count a b = loop a b 0
    where
        loop a b acc | a <=. b = if isCorrect a then loop (inc a) b acc+1 else loop (inc a) b acc
                     | otherwise = acc

main =
    print $ count (1, 9, 3, 6, 5, 1) (6, 4, 9, 7, 2, 9)

