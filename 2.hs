
evenFibonacciNumbers :: Int
evenFibonacciNumbers = evenFibonacciNumbers' 1 2 2

evenFibonacciNumbers' :: Int -> Int -> Int -> Int
evenFibonacciNumbers' prepre pre res =
    if (pre > 4000000)
    then res
    else do
        let x = pre + prepre
        if (x `mod` 2 == 0)
        then evenFibonacciNumbers' pre x (res + x)
        else evenFibonacciNumbers' pre x res

-- *Main> evenFibonacciNumbers
-- 4613732
