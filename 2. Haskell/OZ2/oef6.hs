{-
    6. Pascal's Triangle
-}

row :: [Integer] -> [Integer]
row (x : xs) = go 0 (x : xs) -- hou de vorige bij (in het begin 0)
  where
    go vorig [] = [vorig] -- einde bereikt
    go vorig (x : xs) = (vorig + x) : go x xs

{-
    ZIP
        -> zip [1,2,3] [4,5,6] = [(1,4), (2,5), (3,6)]
        -> kan gebruikt worden om indices aan elk item in een lijst te geven

    ZIP WITH
        -> uitbreiding van zip
           (a -> b -> c) -> [a] -> [b] -> [c]
        -> Pas functie toe op elementen van type a en b om die van type c te bekomen

    zip = zipWith (,)
-}

-- alternatief met zipWith

row' :: [Integer] -> [Integer]
row' xs = zipWith (+) (0 : xs) (xs ++ [0])

{-
    ghci> row [1,1]
        [1,2,1]
    ghci> row [1,2,1]
        [1,3,3,1]
    ghci> row $ row [1,2,1]
        [1,4,6,4,1]
-}

-- met list comprehension
pascal :: [[Integer]]
pascal = [1] : [row k | k <- pascal]

-- met map
pascal' :: [[Integer]]
pascal' = [1] : map row pascal'

-- BINOMIAAL COEFFICIENT
-- (n choose k) = (^n _k) = aantal manieren om k elementen te kiezen uit n
-- (k + 1)th element of the (n + 1)th row

-- Index operator is (!!)

bincoeff :: Int -> Int -> Integer
bincoeff n k = (pascal !! n) !! k