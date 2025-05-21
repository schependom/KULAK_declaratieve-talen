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

odds' :: [Int]
odds' = [2 * k + 1 | k <- [0 ..]]

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

odds'' :: [Integer]
odds'' = genOdds 1
  where
    genOdds n = n : genOdds (n + 2)

-- a2.

pythagorean :: [(Int, Int, Int)]
pythagorean =
  [ (a, b, c)
    | m <- [2 ..],
      n <- [1 .. m - 1],
      even m || even n,
      odd m || odd n,
      ggd m n == 1, -- coprime
      let a = m ^ 2 - n ^ 2,
      let b = 2 * m * n,
      let c = m ^ 2 + n ^ 2
  ]

ggd :: Int -> Int -> Int
ggd x y = go $ max x y
  where
    go n
      | x `mod` n == 0 && y `mod` n == 0 = n
      | otherwise = go (n - 1)

ggd' :: Int -> Int -> Int
ggd' x y
  | x == y = x
  | x < y = ggd' y x
  | otherwise = ggd' y (x - y)

-- b1.
-- versie 1: partiele sommen op basis lijsten indices
pIndexSums :: (Num a) => [a] -> [a]
pIndexSums l = [sum (take k l) | k <- indices l]

-- genereer een lijst van mogelijke indices in de gegeven lijst
-- de lijst kan ook oneindig zijn
indices :: [a] -> [Int]
indices [] = []
indices (x : xs) = 1 : map (+ 1) (indices xs)

indices' :: [a] -> [Int]
-- foldl f z l
--    f :: r -> a -> a
--    z :: r
--    l :: [a]
--  res :: r
-- (0 + a)
indices' l = reverse $ foldl (\r _ -> succ (head r) : r) [1] l

-- versie 2: partiele sommen op basis van prefix lijsten
pPrefixSums :: (Num a) => [a] -> [a]
pPrefixSums l = [sum p | p <- prefixes l]

-- genereer een lijst van mogelijke prefixes van de gegeven lijst
-- de lijst kan ook oneindig zijn
prefixes :: [a] -> [[a]]
-- foldr f z l
--    f :: a -> r -> a
--    z :: r
--    l :: [a]
--  res :: r
prefixes l = foldr (\a rl -> [a] : map (a :) rl) [] l -- !!

-- [1,2,3]
-- a = 3, rl = []             -> [3] : []               -> [[3]]
-- a = 2, rl = [[3]]          -> [2] : [[2,3]]          -> [[2], [2,3]]
-- a = 1, rl = [[2], [2,3]]   -> [1] : [[1,2], [1,2,3]] -> [[1], [1,2], [1,2,3]]

-- versie 3: onmiddellijk de partiele sommen genereren
partialSums :: (Num a) => [a] -> [a]
partialSums = foldr (\a r -> a : map (+ a) r) [] -- !!

-- niet goed voor infinite lists??
partialSums' :: (Num a) => [a] -> [a]
partialSums' x = map sum (partialLists x)
  where
    partialLists [] = []
    partialLists (x : xs) = [x] : map (x :) (partialLists xs)

-- dit alleszins wel!
partialSums'' :: (Num a) => [a] -> [a]
partialSums'' = go 0 -- init acc op 0
  where
    go _ [] = []
    go acc (x : xs) = (acc + x) : go (acc + x) xs

partialSums''' :: (Num a) => [a] -> [a]
partialSums''' = tail . scanl (+) 0

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

-- MODELOPLOSSING:
-- leavePositions k n lijst
-- laat elke n-de positie in lijst vallen
-- begin initieel af te tellen vanaf k en verwijder bij het bereiken van 1
-- leavePositions 1 2 [1,2,3,4,5,6] = [2,4,6]
-- leavePositions 2 2 [1,2,3,4,5,6] = [1,3,5]
leavePositions :: Int -> Int -> [a] -> [a]
leavePositions _ _ [] = []
leavePositions 1 n (x : xs) = leavePositions n n xs
leavePositions k n (x : xs) = x : leavePositions (k - 1) n xs

-- geef alle n-de machten terug
moessner :: Int -> [Integer]
moessner n = moessnerIteration' n [1 ..]

moessnerIteration :: Int -> [Integer] -> [Integer]
moessnerIteration 1 l = l
moessnerIteration k l = moessnerIteration (k - 1) (partialSums' $ removeEveryNth k l)

moessnerIteration' :: Int -> [Integer] -> [Integer]
moessnerIteration' 1 l = l
moessnerIteration' k l = moessnerIteration' (k - 1) ((partialSums' . leavePositions k k) l)