
smallestMultiple :: Int -> Int
smallestMultiple n = foldl (*) 1 $ map (mulX n 1) [x | x <- [2..n], isPrime x]

mulX :: Int -> Int -> Int -> Int
mulX n res x =
    if res * x > n
    then res
    else mulX n (res * x) x

isPrime :: Int -> Bool
isPrime = isPrime' 2

isPrime' :: Int -> Int -> Bool
isPrime' factor n
    | factor * factor > n  = True
    | n `mod` factor == 0 = False
    | n `mod` factor /= 0 = isPrime' (factor + 1) n

-- *Main> smallestMultiple 20
-- 232792560
