-- Recursief gedefinieerde lijst

priem :: [Int]
priem = 2 : [k | k <- [3 ..], nondiv k (takeWhile (\p -> p ^ 2 <= k) priem)]

nondiv :: Int -> [Int] -> Bool
nondiv _ [] = True
nondiv n (x : xs)
  | n `mod` x == 0 = False
  | otherwise = nondiv n xs

-- verwijder veelvouden
verwijderVeelvouden :: Integer -> [Integer] -> [Integer]
verwijderVeelvouden x l = filter (\y -> y `mod` x /= 0) l

verwijderVeelvouden' :: Integer -> [Integer] -> [Integer]
verwijderVeelvouden' x l = [e | e <- l, e `mod` x /= 0]

-- oneindige lijst priemgetallen
-- aan de hand van de zeef van Eratosthenes
allepriem :: [Integer]
allepriem = zeef [2 ..]
  where
    zeef (x : xs) = x : zeef (verwijderVeelvouden x xs)

allepriem' :: [Integer]
allepriem' = zeef [2 ..]
  where
    zeef (x : xs) = x : zeef (filter (\y -> y `mod` x /= 0) xs)

{-
ghci> take 10 $ verwijderVeelvouden 3 [1..]
    [1,2,4,5,7,8,10,11,13,14]

ghci> take 10 allepriem
    [3,4,5,7,11,13,17,19,23,29]
-}