{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

{-
    1. List Operations
-}

count :: [Int] -> Int
count [] = 0
count (h : t) = 1 + count t

-- met foldr
--    waarbij foldr f z [x1, x2, x3] = f x1 (f x2 (f x3 z))
count' :: [Int] -> Int
count' = foldr f' 0 -- al eta gereduceerd! argument l weggelaten!
  where
    f' :: Int -> Int -> Int
    f' _ acc = acc + 1

-- foldr        f            z  lijst
-- foldr :: (e -> r -> r) -> r -> [e] -> r

-- nog korter, met (eta gereduceerde) ANONIEME FUNCTIE! (\lambda x -> ...) is (\ x -> ...) in Haskell
count'' :: [Int] -> Int
count'' = foldr (\_ acc -> acc + 1) 0 -- don't care _ voor eerste argument!

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False : tail) = False
myAnd (True : tail) = myAnd tail

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : tail) = True
myOr (False : tail) = myOr tail

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x : xs)
  | x = True -- zelfde als x == True
  | otherwise = myOr' xs

append :: [Int] -> [Int] -> [Int]
-- basisgeval
append [] lijst = lijst
-- recursief geval
append (x : xs) y = x : append xs y

myProduct :: [Integer] -> Integer
-- basisgeval (!)
myProduct [] = 1
-- recursief geval
myProduct (head : tail) = head * myProduct tail

-- met foldr
myProduct' :: [Integer] -> Integer
myProduct' = foldr (*) 1

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x : xs)
  | n < x = n : x : xs -- insert voorop
  | otherwise = x : insert n xs -- insert in de staart

myLast :: [Int] -> Int
myLast [] = error "You should't provide an empty list!"
myLast [l] = l
myLast (_ : xs) = myLast xs