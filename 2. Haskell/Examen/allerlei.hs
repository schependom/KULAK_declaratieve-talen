import Data.List
import Data.Maybe

erover :: [Integer] -> Integer -> Integer
erover l n = go 0 l n
  where
    go acc (x : xs) n
      | x > n = acc
      | otherwise = go (succ acc) xs n

-- !!
-- !! go kan aan de argumenten van de main functie!!

erover'' :: [Integer] -> Integer -> Integer
erover'' (x : xs) n = go 0 -- begin bij index 0
  where
    go acc
      | acc > x = acc
      | otherwise = go $ succ acc

{-------------
x `elem` l checkt of x een element is van de lijst (of string!) l!!
--------------}

countStringsThatEndInDigits :: [String] -> Int
countStringsThatEndInDigits lijst = length $ filter endsInDigit lijst
  where
    endsInDigit :: String -> Bool
    endsInDigit s = last s `elem` "012"

endsInOneOfDigits :: String -> String -> Bool
endsInOneOfDigits s d = last s `elem` d

{-------------
findIndex, elemIndex en lookup uit Array.List
--------------}

-- Char met ''
-- String met ""

-- ! findIndex :: (a -> Bool) -> [a] -> Maybe Int
-- findIndex isSpace "Hello World!"
-- Just 5

-- ! elemIndex :: a -> [a] -> Maybe Int

-- lookup
-- ! lookup :: Eq a => a -> [(a, b)] -> Maybe b

rhymes :: [String] -> [String] -> Bool
rhymes l1 l2 =
  drop i1 l1 == drop i2 l2
  where
    i1 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") l1 -- FIND INDEX
    i2 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") l2

rhymes' :: [String] -> [String] -> Bool
rhymes' l1 l2 =
  drop i1 l1 == drop i2 l2
  where
    i1 = fromMaybe 0 $ elemIndex "0" l1 -- ELEM INDEX
    i2 = fromMaybe 0 $ elemIndex "0" l2

valueOf :: [(String, Int)] -> String -> Int
valueOf lijst s = fromJust (lookup s lijst)

{-------------
all & alle paren genereren van een lijst adhv list comprehensions
--------------}

-- controleer of voor elk paar (x,y) geldt dat x GEEN prefix is van y
noPrefix :: (Eq a) => [[a]] -> Bool
noPrefix xs = all (\(x, y) -> not (isPrefix x y)) [(x, y) | x <- xs, y <- xs, x /= y]

-- is lijst1 de prefix van lijst2?
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix l1 l2 = take (length l1) l2 == l1

{-------------
GEEN 'IN' NODIG BIJ 'LET' in DO-block!
--------------}

{-------------
LIST COMPREHENSIONS
--------------}

flatten :: [[Int]] -> [Int]
flatten xss = [x | xs <- xss, x <- xs]

kleinerDanN :: Int -> [Int]
kleinerDanN n = [x ^ 2 | x <- takeWhile (< n) [0 ..]]

{-------------
GGD
--------------}

-- assume x > y zodat je (x-y) kan uitvoeren!
ggd :: Int -> Int -> Int
ggd x y
  | x == y = x
  | x < y = ggd y x
  | otherwise = ggd y (x - y) -- x > y

{-------------
partial sums
--------------}

-- foldr f z l
myFoldr :: (e -> r -> r) -> r -> [e] -> r
myFoldr _ z [] = z
myFoldr f z (x : xs) = x `f` myFoldr f z xs

-- foldl f z l
myFoldl :: (r -> e -> r) -> r -> [e] -> r
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (acc `f` x) xs

partialSums :: (Num a) => [a] -> [a]
partialSums l = myFoldr (\e r -> e : map (+ e) r) [] l -- !!