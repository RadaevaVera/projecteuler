
multiplesOf3Or5 :: Int -> Int
multiplesOf3Or5 n = foldr (+) 0
  [ x | x <- [1..(n - 1)], (x `mod` 3 == 0) || (x `mod` 5 == 0) ]

-- *Main> multiplesOf3Or5 1000
-- 233168
