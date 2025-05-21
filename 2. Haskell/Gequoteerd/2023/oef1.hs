-- 1.1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

erover :: [Integer] -> Integer -> Integer
erover l n = go 0 l n
  where
    go acc (x : xs) n
      | x > n = acc
      | otherwise = go (succ acc) xs n

-- !!
-- de go kan aan de argumenten van erover!!

erover'' :: [Integer] -> Integer -> Integer
erover'' (x : xs) n = go 0 -- begin bij index 0
  where
    go acc
      | acc > x = acc
      | otherwise = go $ succ acc

-- modeloplossing: zonder accumulator
erover' :: [Integer] -> Integer -> Integer
erover' (x : xs) grens
  | x > grens = 0
  | otherwise = 1 + erover xs grens

{-

ghci> erover [1..] 5
5

ghci> erover [x*x*x | x <- [1..]] 10000000000
2154

-}

-- 1.2
erover_f :: (Ord a) => (Int -> a) -> Int -> a -> Int
erover_f f onder boven = go onder
  where
    go :: Int -> Int
    go acc
      | f acc > boven = acc
      | otherwise = go (succ acc)

-- !!
-- de go kan aan de argumenten van erover!!

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
-- 0  2   4   6   8     10    12    14  (getallen)
-- 0  1   1       2                 3   (fibonacci van de overlopen indices)

-- Telkens een fibonacci getal tellen bij de vorige index om de volgende te verkrijgen

-- 0, (0+1=1), (1+1=2), (2+2=4), (4+3=7)

-- 1.3
fiberover :: (Ord a) => (Int -> a) -> Int -> a -> Int
fiberover f van grens =
  let indexNietErover = go van 1 1 -- zoek de laatste index vóór we erover gaan
   in if f (indexNietErover + 1) > grens -- stop als het verschil in index gelijk is aan 1
        then indexNietErover + 1
        else fiberover f indexNietErover grens
  where
    -- als we erover gaan, return de index ervoor -> laatste index die lager of gelijk is
    go index fib1 fib2
      | f (index + fib1) > grens = index
      | otherwise = go (index + fib1) fib2 (fib1 + fib2)

{-

ghci> fiberover (\x -> x*x*x*x*x*x) 0 1000000000
32

ghci> fiberover (\x -> x*x) 0 1000000000
31623

ghci> fiberover (\x -> x) 0 1000000000
1000000001

ghci> fiberover (\x -> (fromIntegral (x*x))) 0 (10000000000000/111)
300151

-}

fiberover_l :: (Ord a) => (Int -> a) -> Int -> a -> (Int, [Int])
fiberover_l f van grens =
  let tupelNietErover = go van 1 1 []
      indexNietErover = fst tupelNietErover
      lijstNietErover = snd tupelNietErover
   in if f (indexNietErover + 1) > grens
        then (indexNietErover + 1, reverse lijstNietErover ++ [indexNietErover + 1])
        else
          let (volgendeIndex, volgendeLijst) = fiberover_l f indexNietErover grens
           in (volgendeIndex, reverse lijstNietErover ++ (indexNietErover + 1 : volgendeLijst))
  where
    go :: Int -> Int -> Int -> [Int] -> (Int, [Int])
    go index fib1 fib2 lijst
      | f (index + fib1) > grens = (index, index + fib1 : lijst)
      | otherwise = go (index + fib1) fib2 (fib1 + fib2) (index + fib1 : lijst)

{-

ghci> fiberover_l (\x -> x) 0 10
(11,[1,2,4,7,12,8,8,9,11,10,10,11,11])

ghci> fiberover_l (\x -> x*x*x) 0 1000000000
(1001,[1,2,4,7,12,20,33,54,88,143,232,376,609,986,1596,987,987,988,990,993,998,1006,999,999,1000,1002,1001])

ghci> fiberover_l (\x -> (fromIntegral (x*x*x))) 0 (10000000000000/111)
(4483,[1,2,4,7,12,20,33,54,88,143,232,376,609,986,1596,2583,4180,6764,4181,4181,4182,4184,4187,4192,4200,4213,4234,4268,4323,4412,4556,4413,4413,4414,4416,4419,4424,4432,4445,4466,4500,4467,4467,4468,4470,4473,4478,4486,4479,4479,4480,4482,4485,4483])

-}