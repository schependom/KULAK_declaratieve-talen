{-
    4. Lists, Ranges and List Comprehensions
-}

-- Range met behulp van list comprehensions
range :: Int -> Int -> [Int]
range a b = [a .. b]

-- Recursieve range
range' :: Int -> Int -> [Int]
range' a b
  | a > b = []
  | otherwise = a : range' (a + 1) b

-- Point-less (geen argumenten) alias
(.$) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g = \x -> f (g x)                  (zoals HC)
(.$) f g x = f (g x)

-- Factorial
factorial :: Integer -> Integer
factorial n = product [1 .. n]

factorial' n = product . range (1 n)

factorial'' n = product [1 .. n]

-- myRepeat
myRepeat :: Int -> Int -> [Int]
myRepeat x n = [x | _ <- [1 .. n]]

-- flatten
flatten :: [[Int]] -> [Int]
flatten l = [x | xs <- l, x <- xs] -- !

flatten' :: [[Int]] -> [Int]
-- flatten' l = foldr (++) [] l
flatten' = foldr (++) [] -- eta gereduceerd!
-- dit is gelijk aan concat

flatten'' :: [[Int]] -> [Int]
flatten'' = concat -- eta gereduceerd!

sumInts :: Int -> Int -> Int
sumInts l h = sum [l .. h]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples x l = [y | y <- l, y `mod` x /= 0]