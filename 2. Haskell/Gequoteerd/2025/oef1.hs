-- 1
-- We gebruiken Integer omdat faculteiten heel groot kunnen worden...
fac :: Integer -> Integer
fac n = product [1 .. n]

catalan :: Integer -> Integer
catalan n = fromInteger $ div (fac (2 * n)) (fac (n + 1) * fac n)

-- 2
catalanRec :: Integer -> Integer
catalanRec 0 = 1 -- basisgeval
catalanRec npe =
  -- npe is (n+1)
  let n = npe - 1
   in div (2 * (2 * n + 1) * catalanRec n) (n + 2)

-- 3
--    implementatie 1, waarbij de formule herschreven werd met n als argument ipv (n+1)
catalanSum :: Integer -> Integer
catalanSum 0 = 1 -- basisgeval
catalanSum n = sum [catalanSum i * catalanSum (n - 1 - i) | i <- [0 .. n - 1]]

--    implemenatie 2, waarbij het argument (n+1) is, zoals in de opgave
catalanSum' :: Integer -> Integer
catalanSum' 0 = 1
catalanSum' npe =
  let n = npe - 1
   in sum [catalanSum' i * catalanSum' (n - i) | i <- [0 .. n]]

-- 4.
catalanAsy :: Integer -> Double
catalanAsy n = (4 ^ n) / (fromIntegral n ** 1.5 * sqrt pi)

-- 5.
--    f is een functie met als domein de natuurlijke getallen
--    tabulatie zet de resultaten van deze functie, toegpast op de natuurlijke getallen, in een tabel
tabulatie :: (Integer -> a) -> [a]
tabulatie f = map f [0 ..]

{- VB:
    ghci> take 10 $ tabulatie (*2)
    [0,2,4,6,8,10,12,14,16,18]
-}

-- lcatalan past de verschillende catalan functies toe
-- op de natuurlijke getallen aan de hand van tabulatie.

lcatalan :: [Integer]
lcatalan = tabulatie catalan

lcatalanRec :: [Integer]
lcatalanRec = tabulatie catalanRec

lcatalanSum :: [Integer]
lcatalanSum = tabulatie catalanSum

-- 6.
odds :: [Integer] -> [(Integer, Integer)]
odds c = filter (odd . snd) (zip [0 ..] c) -- snd is het catalangetal van het tupel, dat moet oneven zijn

odds' :: [Integer] -> [(Integer, Integer)]
odds' l = filter (\(_, x) -> odd x) (zip [0 ..] l)

-- als oneindige lijst...
oddsInf :: [(Integer, Integer)]
oddsInf = odds lcatalanRec

{- VB.
    ghci> take 6 $ oddsInf
    [(0,1),(1,1),(3,5),(7,429),(15,9694845),(31,14544636039226909)]
-}

-- 7.
testCatalan :: Integer -> Integer -> [(Bool, Double)]
testCatalan start einde =
  [(check n, error n) | n <- [start .. einde]]
  where
    -- check of catalan en catalanRec hetzelfde resultaat geven
    check :: Integer -> Bool
    check n = catalan n == catalanRec n
    -- bereken de relatieve fout van de asymptotische benadering
    error :: Integer -> Double
    error n = (abs . (/ exact)) (catalanAsy n - exact)
      where
        exact = fromInteger $ catalanRec n

-- check of catalan en catalanRec hetzelfde resultaat geven
check' :: Integer -> Bool
check' n
  | catalan n == catalanRec n = True
  | otherwise = False