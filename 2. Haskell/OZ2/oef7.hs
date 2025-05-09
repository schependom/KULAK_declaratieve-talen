import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple (fromVersionIntervals)
import GHC.Base

{-
    7. Caeser Cipher (EXTRA)
-}

-- only lowercase
-- all other letters unchanged

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- eigen functies

shift :: Int -> Char -> Char
shift s l
  | isLower l = int2let . (`mod` 26) . (+ s) . let2int $ l
  | otherwise = l

encode :: Int -> String -> String
encode sh str = [shift sh e | e <- str]

{-

ghci> shift (-2) 'a'
'y'

ghci> shift 3 ' '
' '

ghci> encode 3 "haskell is fun"
"kdvnhoo lv ixq"

ghci> encode (-3) "kdvnhoo lv ixq"
"haskell is fun"

-}

-- FREQUENCY TABLES

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- numerator / denomitor = percent
percent :: Int -> Int -> Float
percent n d = 100 * fromIntegral n / fromIntegral d

-- !!
freqs :: String -> [Float]
freqs s = [percent (count l s) (length s) | l <- ['a' .. 'z']]

-- !!
count :: Char -> String -> Int
count c = length . filter (== c)

-- CRACKING THE CIPHER

chisqr :: [Float] -> [Float] -> Float
chisqr o e = sum [(oi - ei) ^ 2 / ei | (oi, ei) <- zip o e]

rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

-- geobserveerde percentages
table' :: [Float]
table' = freqs "kdvnhoo lv ixq"

-- vergelijken met de verwachte percentages
{-
  ghci> [ chisqr (rotate n table') table | n <- [0..25] ]
  [1037.1334,472.56082,452.01575,150.9809,1059.9152,3122.5366,480.62592,857.73126,716.23804,732.0249,367.40616,1094.9907,1689.4485,1036.0013,1097.7847,2231.3276,486.82352,2086.116,724.60504,597.21326,964.7697,626.74554,2138.8533,703.41705,3906.1936,462.55478]
-}

-- met de functies:
--    minimum :: Ord a => [a] -> a
--    elemIndex :: Eq a => a -> [a] -> Maybe Int

minIndex' :: (Ord a) => [a] -> Int
minIndex' xs = snd $ minimumBy (\(x1, _) (x2, _) -> compare x1 x2) (zip xs [0 ..])

minIndex :: [Float] -> Int
minIndex l = fromJust $ elemIndex (minimum l) l

-- we weten zeker dat het element in de lijst zit!
-- dus we kunnen fromJust gebruiken
-- want elemIndex returnt een Maybe Int

-- 1. bereken de frequenties van de string s
-- 2. bereken het minimum onder het aantal shift posities t.o.v. de chisq van de frequentietabellen
-- 3. decode met encode(-)

crack :: String -> String
crack s =
  let table'' = freqs s -- 1
   in let i = minIndex [chisqr (rotate n table'') table | n <- [0 .. 25]] -- 2
       in encode (-i) s -- 3

-- mooier:

crack' :: String -> String
crack' s = encode (-i) s ++ ", i was " ++ show i
  where
    observedFreqs = freqs s
    i = minIndex [chisqr (rotate n observedFreqs) table | n <- [0 .. 25]]

{-

  ghci> crack' "kdvnhoo lv ixq"
  "haskell is fun"

  ghci> crack' "vscd mywzboroxcsyxc kbo ecopev"
  "list comprehensions are useful, i was 10"

  ghci> encode 10 "list comprehensions are useful"
  "vscd mywzboroxcsyxc kbo ecopev"

-}