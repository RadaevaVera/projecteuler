
_10001stPrime :: Int -> Int
_10001stPrime n = head $ reverse $ take n [x | x <- [2..], isPrime x]

isPrime :: Int -> Bool
isPrime = isPrime' 2

isPrime' :: Int -> Int -> Bool
isPrime' factor n
    | factor * factor > n  = True
    | n `mod` factor == 0 = False
    | n `mod` factor /= 0 = isPrime' (factor + 1) n

-- *Main> _10001stPrime 10001
-- 104743
