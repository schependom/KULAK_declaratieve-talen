{-
    1. Folds
-}

{-
    1.1 I Did It My Way
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

{-
    1.2 Associativity and Folds
-}

-- own foldl and foldr

-- fold left (associate to the left) ==> ((((0−1)−2)−3)−4) = −10
myFoldl :: (r -> e -> r) -> r -> [e] -> r
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

{-
    foldl (+) 0 [1,2,3]
        foldl (+) (0+1) [2,3]
        foldl (+) 1 [2,3]
        foldl (+) (1+2) [3]
        foldl (+) 3 [3]
        foldl (+) (3+3) []
        foldl (+) 6 []
        6
-}

-- fold right (associate to the right) => (1−(2−(3−(4−0)))) =−2
myFoldr :: (e -> r -> r) -> r -> [e] -> r
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

{-
    foldr (+) 0 [1,2,3]
        1 + (foldr (+) 0 [2,3])
        1 + (2 + (foldr (+) 0 [3]))
        1 + (2 + (3 + (foldr (+) 0 [])))
        1 + (2 + (3 + 0))
        6
-}

-- foldl f z l
-- f :: r -> e -> r

readInBase :: Int -> [Int] -> Int
-- via Horners methode
-- 130_6 = ((0 * 6 + 1) * 6 + 3) * 6 + 0
readInBase n l = myFoldl f 0 l
  where
    -- f :: r -> e -> r
    f acc e = acc * n + e

{-
    1.3 Map
-}

-- map :: (a->b) -> [a] -> [b]

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = f x : myMap f xs

myMapF :: (a -> b) -> [a] -> [b]
-- foldr f z l                      -> resultaat
-- foldr (e -> r -> r) -> r -> [e]  -> r
--
-- We willen iets van de vorm
--      h l = foldr f z l
-- Met
--      f :: e -> r -> r
-- Want (+) 5 0 = 5 bij het geval van 'plus [5]'

-- Hier geldt
--      - h = myMapF
--      - z = [] want map f [] = []
--      - f? Neemt als argumenten:
--              element (type e)
--              basis (type r)
--           Output
--              resultaat (type r)

-- g is de functie die we willen applyen, bv. toUpper
-- z was [], zoals hierboven gezegd
myMapF g l = foldr f [] l
  where
    -- f :: e -> r -> r
    f el ba = g el : ba

-- kan nog gereduceerd worden met eta reduce