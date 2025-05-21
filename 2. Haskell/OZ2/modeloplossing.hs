import Data.Char
import Data.List
import Data.Maybe

-- | 1 Folds
-- | I did it My way

{-
  mySum::[Integer] -> Integer

  'mySum list' is de som van de elementen in list
-}
mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

{-
  myProduct::[Integer] -> Integer

  'myProduct list' is het product van de elementen in list
-}
myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

{-
  foldInts::(Integer -> Integer -> Integer)
            -> Integer -> [Integer] -> Integer

  'foldInts f e l' past f toe tussen alle elementen
  in de lijst l en geeft het resultaat terug
-}
foldInts ::
  (Integer -> Integer -> Integer) ->
  Integer ->
  [Integer] ->
  Integer
foldInts f e [] = e
foldInts f e (x : xs) = x `f` foldInts f e xs

-- | Associativiteit en folds

{-
  myFoldl::(b -> a -> b) -> b -> [a] -> b

  myFoldl f e [x,y,z,...] = ((((e `f` x) `f` y) `f` z) `f` ...)
-}
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f e [] = e
myFoldl f e (x : xs) = myFoldl f (e `f` x) xs -- !!

{-
  myFoldr::(a -> b -> b) -> b -> [a] -> b

  myFoldr f e [x,y,z,...] = (x `f` (y `f` (z `f` (... `f` e))))
-}
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f e [] = e
myFoldr f e (x : xs) = x `f` myFoldr f e xs

{-
  readInBase::Int -> [Int] -> Int

  'readInBase b [d]' is de waarde in basis b van het getal
  voorgesteld in de lijst van cijfers [d]
  van cijfers
-}
readInBase :: Int -> [Int] -> Int
readInBase b = myFoldl (\x y -> y + b * x) 0

-- | Map

{-
  myMap::(a -> b) -> [a] -> [b]

  'myMap f [x,y,z,...]' is [f x,f y,f z,...]
-}
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

-- | 2 Functie ketens

{-
  applyAll::[a -> a] -> a -> a

  applyAll past een lijst van functies toe op het argument
  eerst de laatste, dan de voorlaatste enz.
-}
applyAll :: [a -> a] -> a -> a
applyAll = myFoldr (.) id

{-
  applyTimes::Int -> (a -> a) -> a -> a

  applyTimes past een functie een aantal keer toe
  als aantal <= 0 wordt ze 0 keer toegepast
-}
applyTimes :: Int -> (a -> a) -> a -> a
applyTimes aantal f = applyAll [f | _ <- [1 .. aantal]]

{-
  applyMultipleFuncs::a->[a->b]->[b]

  applyMultipleFuncs past een lijst functies toe op een
  argument en geeft de lijst van resulaten terug
-}
applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x lijst = [f x | f <- lijst]

-- | List comprehension

{-
  mapLC::(a->b) -> [a] -> [b]

  mapLC past een functie toe op elk element van een lijst door listcomprehension
-}
mapLC :: (a -> b) -> [a] -> [b]
mapLC f list = [f x | x <- list]

{-
  filterLC::(a -> Bool) -> [a] -> [a]

  filterLC filtert deze element uit de lijst waarvoor de functie waar geeft
-}
filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f lijst = [v | v <- lijst, f v]

-- | 4 Oneindige Lijsten

{-
  odds::[Int]
  Alle oneven getallen
-}
odds :: [Int]
odds = [1 + 2 * x | x <- [0 ..]]

{-
  De lijst van pythgoreaanse drietallen
-}
pythagorean :: [(Int, Int, Int)]
pythagorean =
  [ (m * m - n * n, 2 * m * n, m * m + n * n)
    | m <- [1 ..],
      n <- [1 .. (m - 1)], -- dit bepaalt de volgorde!
      ggd m n == 1,
      even m || even n,
      odd m || odd n
  ]

ggd :: Int -> Int -> Int
ggd x y
  | x == y = x
  | x < y = ggd y x
  | otherwise = ggd y (x - y)

{-
  partialSums::Num a => [a] -> a

  genereer de partiele sommen uit de lijst a
-}
-- versie 1: partiele sommen op basis lijsten indices
pIndexSums :: (Num a) => [a] -> [a]
pIndexSums l = [sum (take k l) | k <- indices l]

-- genereer een lijst van mogelijke indices in de gegeven lijst
-- de lijst kan ook oneindig zijn
indices :: [a] -> [Int]
indices [] = []
indices (x : xs) = 1 : map (+ 1) (indices xs)

indices' :: [a] -> [Int]
-- foldl f z l
--    f :: r -> a -> a
--    z :: r
--    l :: [a]
--  res :: r
-- (0 + a)
indices' l = reverse $ foldl (\r _ -> succ (head r) : r) [1] l

-- versie 2: partiele sommen op basis van prefix lijsten
pPrefixSums :: (Num a) => [a] -> [a]
pPrefixSums l = [sum p | p <- prefixes l]

-- genereer een lijst van mogelijke prefixes van de gegeven lijst
-- de lijst kan ook oneindig zijn
prefixes :: [a] -> [[a]]
-- foldr f z l
--    f :: a -> r -> a
--    z :: r
--    l :: [a]
--  res :: r
prefixes l = myFoldr (\a rl -> [a] : map (a :) rl) [] l -- kan je indices ook zo schrijven?

-- [1,2,3]
-- a = 3, rl = []             -> [3] : []               -> [[3]]
-- a = 2, rl = [[3]]          -> [2] : [[2,3]]          -> [[2], [2,3]]
-- a = 1, rl = [[2], [2,3]]   -> [1] : [[1,2], [1,2,3]] -> [[1], [1,2], [1,2,3]]

-- versie 3: onmiddellijk de partiele sommen genereren
partialSums :: (Num a) => [a] -> [a]
partialSums = myFoldr (\x y -> x : map (+ x) y) []

-- |  De stelling van Moessner

{-
  Als je vertrekt van de lijst opeenvolgende natuurlijke getallen
  [1,2,3,4,5,...]
  en daarop n keer de twee volgende handelingen uitvoert (samenstellen van functies)
    - laat de elementen op (k x n)-de posities weg
    voor n = 2: [1,3,5,...]
    - bepaal de rij van de partiele sommen
    voor n = 2: [1,4,9,...]
  dan bekom je de rij van de opeenvolgende n-de machten
-}
moessner :: Int -> [Integer] -> [Integer]
moessner 1 lijst = lijst
moessner n lijst = moessner (n - 1) ((partialSums . leavePositions n n) lijst)

-- leavePositions k n lijst
-- laat elke n-de positie in lijst vallen
-- begin initieel af te tellen vanaf k en verwijder bij het bereiken van 1
-- leavePositions 1 2 [1,2,3,4,5,6] = [2,4,6]
-- leavePositions 2 2 [1,2,3,4,5,6] = [1,3,5]
leavePositions :: Int -> Int -> [a] -> [a]
leavePositions _ _ [] = []
leavePositions 1 n (x : xs) = leavePositions n n xs
leavePositions k n (x : xs) = x : leavePositions (k - 1) n xs

-- | gebruik moessner om de eerste 5 300-ste machten te berekenen

-- | 5 Hamming numbers

{-
  merge::Ord a => [a] -> [a] -> [a]

  Meng twee int stijgende volgorde gesorteerde lijsten
  zodanig dat dubbels uit verschillende lijsten niet dubbel
  voorkomen in het resultaat
-}
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x == y = x : merge xs ys
  | otherwise = y : merge (x : xs) ys

{-
  hamming::[Integer]

  De lijst van hamminggetallen H is de merge van
  de drie lijsten 2H, 3H, 5H
-}
hamming :: [Integer]
hamming = 1 : merge (map (* 2) hamming) (merge (map (* 3) hamming) (map (* 5) hamming))

{-
  testHamming::Integer -> (Integer, Double, Double)

  test de benadering voor het aantal hamminggetallen < n:
        ln(n*sqrt(30))^3/6/ln(2)/ln(3)/ln(5)
  met een correctie die groeit als log(n)
-}
testHamming :: Integer -> (Integer, Double, Double)
testHamming n = (go n hamming, ap n, log (fromIntegral n))
  where
    go :: Integer -> [Integer] -> Integer
    go n (x : xs) =
      if n < x
        then 1
        else 1 + go n xs
    ap :: Integer -> Double
    ap n = log (fromIntegral n * sqrt 30.0) ** 3 / 6 / log 2.0 / log 3.0 / log 5.0

-- TODO

-- | probeer testHamming met n = 1,10,100,1000,10000,100000,1000000,10000000,100000000,...
-- | kan je go vervangen door een lambda-uitdrukking?
-- | en ap?

-- | 6 Pascal's Triangle

{-
  row::[Integer] -> [Integer]

  Genereer de volgende rij van de driehoek op basis
  van de vorige
-}
row :: [Integer] -> [Integer]
row [] = [1]
row (r : rs) = (1 : zipWith (+) (r : rs) rs) ++ [1]

{-
  pascal::[[Integer]]

  De driehoek van Pascal
-}
pascal :: [[Integer]]
pascal = [1] : map row pascal

{-
  The Binomial Coefficients are the elements
  of Pascal's triangle
-}
bincoeff :: Int -> Int -> Integer
bincoeff n k = pascal !! n !! k

-- | bereken (x + y) ^ n
-- | met behulp van bincoeff
biTPow1 :: Integer -> Integer -> Int -> Integer
biTPow1 x y n = sum [bincoeff n k * (x ^ k) * (y ^ (n - k)) | k <- [0 .. n]]

-- | bereken (x + y) ^ n
-- | rechtstreeks gebruik makend van de rij in pascal
biTPow2 :: Integer -> Integer -> Int -> Integer
biTPow2 x y n = sum (zipWith (*) (pascal !! n) [x ^ k * y ^ (n - k) | k <- [0 .. n]])

-- | bereken (x + y) ^ n
-- | op de meest voor de hand liggende wijze
biTPow :: Integer -> Integer -> Int -> Integer
biTPow x y n = (x + y) ^ n

-- | Ceasar's Cipher

{-
  Zet een kleine letter om in een getal
  tussen 0 ('a') en 25 ('z') en omgekeerd
-}

let2int :: Char -> Int
let2int a = ord a - ord 'a'

int2let :: Int -> Char
int2let o = chr (o + ord 'a')

{-
  shift::Int -> Char -> Char

  Shift een kleine letter over
  een aantal posities
  Laat alle andere tekens onveranderd
-}

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let (mod (let2int c + n) 26)
  | otherwise = c

{-
  Encodeer een string met behulp van een shift factor
-}
encode :: Int -> String -> String
encode s = map (shift s)

-- | Frequency Tables

{-
  de percent functie
-}
percent :: Int -> Int -> Float
percent a b = fromIntegral a / fromIntegral b

{-
  De frekwentie waaraan een kleine letter voorkomt
-}
freq :: String -> [Float]
freq s = go (cnt s (replicate 26 0.0))
  where
    go :: [Float] -> [Float]
    go fs = map (/ sum fs) fs
    cnt :: String -> [Float] -> [Float]
    cnt [] fs = fs
    cnt (x : s) fs =
      if isLower x
        then cnt s (increase (let2int x) fs) -- increase op de juiste index
        else cnt s fs

increase :: (Num a) => Int -> [a] -> [a]
increase _ [] = []
increase 0 (x : xs) = (x + 1) : xs
increase p (x : xs) = x : increase (p - 1) xs

-- | Cracking the cipher

{-
  De Chi kwadraat functie
-}
chisqr :: [Float] -> [Float] -> Float
chisqr o e = sum (zipWith (\x y -> (x - y) ** 2 / y) o e)

{-
  De rotatie
-}
rotate :: Int -> [a] -> [a]
rotate r l = drop r l ++ take r l

{-
  En breek de code
-}
crack :: String -> String
crack s = encode (-shift) s
  where
    shift = fromMaybe 0 (elemIndex minChiSqr chisqrs)
      where
        chisqrs = [chisqr (rotate n table') table | n <- [0 .. 25]]
          where
            table' = freq s
            table =
              [ 8.2,
                1.5,
                2.8,
                4.3,
                12.7,
                2.2,
                2.0,
                6.1,
                7.0,
                0.2,
                0.8,
                4.0,
                2.4,
                6.7,
                7.5,
                1.9,
                0.1,
                6.0,
                6.3,
                9.1,
                2.8,
                1.0,
                2.4,
                0.2,
                2.0,
                0.1
              ]
        minChiSqr = minimum chisqrs

-- | List comprehension diys

{-
  Schrijf versies van de volgende functies zonder comprehension
-}
lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p as = [f a | a <- as, p a]

mnlc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mnlc1 f p as = map f (filter p as)

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = [f a b | a <- as, b <- bf a, p b]

mnlc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
mnlc2 f as bf p = concat (map (\a -> map (f a) (filter p (bf a))) as)

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n =
  [ f a b c | a <- [1 .. n], b <- [a .. n], even a, c <- [b .. n], a * a + b * b == c * c
  ]

mnlc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
mnlc3 f n =
  concat
    ( map
        ( \x ->
            concat
              ( map
                  ( \y ->
                      (map (f x y) (filter (\c -> x * x + y * y == c * c) [y .. n]))
                  )
                  [x .. n]
              )
        )
        (filter even [1 .. n])
    )
