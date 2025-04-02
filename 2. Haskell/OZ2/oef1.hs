{-
    1. Folds
-}

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- foldr f z l  geeft r als resultaat
--                      f                           z           l           r
foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts f z [] = z
foldInts f z (x : xs) = f x (foldInts f z xs)

mySum' = foldInts (+) 0

myProduct' = foldInts (*) 1

-- own foldl and foldr

-- fold left (associate to the left) ==> ((((0−1)−)2−)3−4) =−10
myFoldl :: (r -> e -> r) -> r -> [e] -> r
myFoldl f z [] = z
myFoldl f z (x : xs) = f (myFoldl f z xs) x

-- fold right (associate to the right) => (1−(2−(3−(4−0)))) =−2
myFoldr :: (e -> r -> r) -> r -> [e] -> r
myFoldr f z [] = z
myFoldr f z (x : xs) = f x (myFoldr f z xs)

-- foldl f z l
-- f :: r -> e -> r

readInBase :: Int -> [Int] -> Int
-- via Horners methode
-- 130_6 = (1 * 6 + 3) * 6 + 0
readInBase n l = myFoldl f 0 l
  where
    f r e = r * n + e