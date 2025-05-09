{-
    4. Infinite Lists
-}

{-
    4.1 Operations
-}

-- a1.

-- met [1..] kan je infinite lists maken!
odds :: [Int]
odds = [k | k <- [1 ..], odd k]

{-
    [1..] is syntactic sugar voor enumFrom 1

    [1..]
    == enumFrom 1
    == 1 : 2 : 3 : 4 : 5 : ...

    enumFrom n = n : enumFrom (n+1)
-}

{-
    [a..b] is syntactic sugar voor enumFromTo a b

    [a..b]
    == enumFromTo a b
    == a : a+1 : a+2 : ... : b-1 : b

    enumFromTo a b
        | from > to = []
        | otherwise = a : enumFromTo (succ a) b

    waarbij otherwise een leesbare versie is van True!!
-}

odds' = genOdds 1
  where
    genOdds n = n : genOdds (n + 2)

-- a2.

pythagorean :: [(Int, Int, Int)]
pythagorean =
  [ (a, b, c)
    | m <- [2 ..],
      n <- [1 .. m - 1],
      odd (m - n), -- opposite parity
      gcd m n == 1, -- coprime
      let a = m ^ 2 - n ^ 2,
      let b = 2 * m * n,
      let c = m ^ 2 + n ^ 2
  ]

-- b1.

-- niet goed voor infinite lists??
partialSums :: (Num a) => [a] -> [a]
partialSums x = map sum (partialLists x)
  where
    partialLists [] = []
    partialLists (x : xs) = [x] : map (x :) (partialLists xs)

-- dit alleszins wel!
partialSums' :: (Num a) => [a] -> [a]
partialSums' = go 0 -- init acc op 0
  where
    go _ [] = []
    go acc (x : xs) = (acc + x) : go (acc + x) xs

partialSums'' :: (Num a) => [a] -> [a]
partialSums'' = tail . scanl (+) 0

{-
    4.2 Moessner's Theorem
-}

-- elk n-de getal weglaten
-- en partiële sommen nemen
-- herhalen tot n is 2
-- levert de n-de machten!

-- bv. elk 2de getal (even) weglaten
-- en partiële sommen nemen
-- levert [1, 4, 9, 16, ...] -> kwadraten!

removeEveryNth :: Int -> [a] -> [a]
removeEveryNth n xs = [x | (i, x) <- zip [1 ..] xs, i `mod` n /= 0]

-- geef alle n-de machten terug
moessner :: Int -> [Integer]
moessner n = moessnerIteration n [1 ..]

moessnerIteration :: Int -> [Integer] -> [Integer]
moessnerIteration 1 l = l
moessnerIteration k l = moessnerIteration (k - 1) (partialSums' $ removeEveryNth k l)