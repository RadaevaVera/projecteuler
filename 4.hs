largestPalindromeProduct :: Int
largestPalindromeProduct = foldl max 0 $ map mulOnList [999,998..100]

mulOnList :: Int -> Int
mulOnList x = 
    let res = filter isPalindrom  $ map (\y -> x * y) [999,998..x]
    in
        if (null res)
        then 0
        else head res

isPalindrom :: Int -> Bool
isPalindrom n = n == reverseNumber n 0

reverseNumber :: Int -> Int -> Int
reverseNumber 0 res = res
reverseNumber n res = reverseNumber (n `div` 10) (res * 10 + n `mod` 10)

-- *Main> largestPalindromeProduct
-- 906609
