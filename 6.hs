
sumSquareDifference :: Int -> Int
sumSquareDifference n = foldl (+) 0 $ map (mulOnList n) [1..n]

mulOnList :: Int -> Int -> Int
mulOnList n x = foldl (+) 0 $ map (\y -> 2 * x * y) [(x + 1)..n]

-- *Main> sumSquareDifference 1000
-- 250166416500
