specialPythagoreanTriplet :: Int
specialPythagoreanTriplet = (\(a, b, c) -> a * b * c) $ head $
  [(a, b, c) | b <- [2..499],  c <- [334..997],
    let a = 1000 - b - c, a < b && b < c && a*a + b*b == c*c]

-- python:
--
-- res = 0
-- for c in range (334,997):
-- 	for b in range (2, c):
-- 		a = 1000 - b - c
-- 		if a < 1: break
-- 		if b * b + a * a == c * c:
-- 			res = a * b * c
-- 			break
-- 	if res: break
-- print(res)

-- *Main> specialPythagoreanTriplet
-- 31875000
