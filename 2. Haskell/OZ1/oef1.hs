{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

{-
    1. List Operations
-}

count :: [Int] -> Int
count [] = 0
count (h : t) = 1 + count t

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False : tail) = False
myAnd (True : tail) = myAnd tail

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : tail) = True
myOr (False : tail) = myOr tail

append :: [Int] -> [Int] -> [Int]
-- basisgeval
append [] lijst = lijst
-- recursief geval
append (head1 : tail1) lijst2 = head1 : append tail1 lijst2

myProduct :: [Integer] -> Integer
-- basisgeval (!)
myProduct [] = 1
-- recursief geval
myProduct (head : tail) = head * myProduct tail

insert :: Int -> [Int] -> [Int]
insert getal [] = [getal]
insert getal (head : tail)
  | getal < head = getal : head : tail -- insert voorop
  | otherwise = head : insert getal tail -- insert in de staart

myLast :: [Int] -> Int
myLast [] = error "You should't provide an empty list!"
myLast [l] = l
myLast (h : t) = myLast t