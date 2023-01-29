summationOfPrimes :: Int
summationOfPrimes = sum primes

primes :: [Int]
primes = 2 : filter (\n -> all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..1999999]

-- *Main> summationOfPrimes
-- 142913828922
