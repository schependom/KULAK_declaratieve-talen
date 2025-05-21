{-
    6. Approximating \pi (EXTRA)
-}

-- no explicit recursion, only list comprehensions/ranges

-- sum and product

sumf :: [Float] -> Float
sumf l = foldl (+) 0 l

sumf' :: [Float] -> Float
sumf' = sum

sumf'' :: [Float] -> Float
sumf'' = foldr (+) 0

productf :: [Float] -> Float
productf l = foldr (*) 1 l

productf' :: [Float] -> Float
productf' = foldl (*) 1

-- approximation 1
-- assuming that n is a natural (floating) number
piSum :: Float -> Float
piSum n = sumf [8 / ((4 * x + 1) * (4 * x + 3)) | x <- [0.0 .. n]] -- kan ook met takeWhile (< n) [0 ..]

piSum' :: Float -> Float
piSum' n = sumf [8 / ((4 * x + 1) * (4 * x + 3)) | x <- takeWhile (< n) [0 ..]]

-- approximation 2
piProd :: Float -> Float
piProd n = 4 * productf [(2 * x + 2) * (2 * x + 4) / (2 * x + 3) ^ 2 | x <- [0.0 .. n]]