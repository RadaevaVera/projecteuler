
largestPrimeFactor :: Int -> Int
largestPrimeFactor n = largestPrimeFactor' 2 n

largestPrimeFactor' :: Int -> Int -> Int
largestPrimeFactor' factor n
    | factor * factor > n  && n == 1 = factor 
    | factor * factor > n  = n
    | n `mod` factor == 0 = largestPrimeFactor' factor (n `div` factor)
    | n `mod` factor /= 0 = largestPrimeFactor' (factor + 1) n

-- *Main> largestPrimeFactor 600851475143
-- 6857
