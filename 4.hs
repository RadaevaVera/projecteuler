
largestPalindromeProduct :: Int
largestPalindromeProduct = head $ reverse $
  [ x * y | x <- [100..999], y <- [100..999], isPalindrom (x * y) ]

isPalindrom :: Int -> Bool
isPalindrom n = n == reverseNumber n 0

reverseNumber :: Int -> Int -> Int
reverseNumber 0 res = res
reverseNumber n res = reverseNumber (n `div` 10) (res * 10 + n `mod` 10)

-- *Main> largestPalindromeProduct
-- 580085
