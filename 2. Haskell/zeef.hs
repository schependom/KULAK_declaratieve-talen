-- oneindige lijst priemgetallen
-- aan de hand van de zeef van Eratosthenes
allepriem :: [Integer]
allepriem = zeef [2..]
    where
        zeef (x:xs) = x : zeef (verwijderVeelvouden x xs)

allepriem' = zeef [2..]
    where 
        zeef (x:xs) = x : (zeef $ filter (\y -> y `mod` x /= 0) xs)

verwijderVeelvouden :: Integer -> [Integer] -> [Integer]
verwijderVeelvouden x l = filter (\y -> y `mod` x /= 0) l

verwijderVeelvouden' :: Integer -> [Integer] -> [Integer]
verwijderVeelvouden' x l = [e | e <- l, e `mod` x /= 0]

{-
ghci> take 10 $ verwijderVeelvouden 3 [1..]
    [1,2,4,5,7,8,10,11,13,14]

ghci> take 10 allepriem
    [3,4,5,7,11,13,17,19,23,29]
-}