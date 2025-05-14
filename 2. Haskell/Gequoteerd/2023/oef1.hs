-- 1.1
erover :: [Integer] -> Integer -> Integer
erover l n = go 0 l n
  where
    go acc (x : xs) n
      | x > n = acc
      | otherwise = go (succ acc) xs n

{-

ghci> erover [1..] 5
5

ghci> erover [x*x*x | x <- [1..]] 10000000000
2154

-}

-- 1.2
erover_f :: (Ord a) => (Int -> a) -> Int -> a -> Int
erover_f f onder boven = go f onder boven
  where
    go :: (Ord a) => (Int -> a) -> Int -> a -> Int
    go f acc boven
      | f acc > boven = acc
      | otherwise = go f (succ acc) boven

erover_f' :: (Ord a) => (Int -> a) -> Int -> a -> Int
erover_f' f acc boven
  | f acc > boven = acc
  | otherwise = erover_f' f (succ acc) boven

{-

ghci> erover_f (\x -> (fromIntegral (x*x))) 0 (10000000000000/111)
300151

ghci> erover_f (\x -> x*x*x*x*x*x) 0 1000000000
32

ghci> erover_f (\x -> x*x) 0 1000000000
31623

ghci> erover_f (\x -> x) 0 1000000000
\^CInterrupted.

ghci> erover_f (\x -> ['a'..'z']!!x) 0 'e'
5

-}

-- Groter dan 11?
-- 0  1   2   3   4     5     6     7   (indices)
-- 2  4   6   8   10    12    14    16  (getallen)
-- 0  1   1       2                 3   (fibonacci van de overlopen indices)

-- Telkens een fibonacci getal tellen bij de vorige index om de volgende te verkrijgen

-- 0, (0+1=1), (1+1=2), (2+2=4), (4+3=7)

-- 1.3
fiberover :: (Ord a) => (Int -> a) -> Int -> a -> Int
fiberover f onder boven = go (map f [onder ..]) 0 0 1 boven
  where
    go l vorigeIndex fibo1 fibo2 grens
      | l !! (vorigeIndex + fibo2) <= grens = go l (vorigeIndex + fibo2) fibo2 (fibo1 + fibo2) grens
      | otherwise =
          if (vorigeIndex + fibo2) - vorigeIndex == 1
            then vorigeIndex + fibo2
            else go l vorigeIndex 0 1 grens

-- 1.4
fiberover_l :: (Ord a) => (Int -> a) -> Int -> a -> (Int, [Int])
fiberover_l f onder boven = go (map f [onder ..]) 0 0 1 boven []
  where
    go l vorigeIndex fibo1 fibo2 grens eval
      | l !! (vorigeIndex + fibo2) <= grens = go l (vorigeIndex + fibo2) fibo2 (fibo1 + fibo2) grens (vorigeIndex + fibo2 : eval)
      | otherwise =
          if (vorigeIndex + fibo2) - vorigeIndex == 1
            then (vorigeIndex + fibo2, vorigeIndex + fibo2 : eval)
            else go l vorigeIndex 0 1 grens (vorigeIndex + fibo2 : eval)