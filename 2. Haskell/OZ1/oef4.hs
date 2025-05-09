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

factorial'' = product . range 1 -- eta gereduceerd!

factorial''' n
  | n < 0 = error "Provide a positive number"
  | n == 0 = 1
  | otherwise = n * factorial''' (n - 1)

-- myRepeat
myRepeat :: Int -> Int -> [Int]
myRepeat n x = [x | _ <- [1 .. n]]

myRepeat' n x
  | n <= 0 = []
  | otherwise = x : myRepeat' n (x - 1)

-- flatten
flatten :: [[Int]] -> [Int]
flatten l = [x | xs <- l, x <- xs] -- !!!

flatten' :: [[Int]] -> [Int]
-- flatten' l = foldr (++) [] l
flatten' = foldr (++) [] -- eta gereduceerd!
-- dit is gelijk aan concat

flatten'' :: [[Int]] -> [Int]
flatten'' = concat -- eta gereduceerd!

sumInts :: Int -> Int -> Int
sumInts l h = sum [l .. h]

sumInts' :: Int -> Int -> Int
sumInts' l h
  | l > h = 0
  | otherwise = l + sumInts (l + 1) h

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples x l = [y | y <- l, y `mod` x /= 0]

removeMultiples' x l = filter (\y -> y `mod` x /= 0) l

removeMultiples'' :: (Integral a) => a -> [a] -> [a]
removeMultiples'' x = filter (\y -> y `mod` x /= 0)

removeMultiples''' x = filter $ (/= 0) . (`mod` x)