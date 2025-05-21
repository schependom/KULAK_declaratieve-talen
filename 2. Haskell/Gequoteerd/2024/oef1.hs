-- 1
-- We gebruiken Integer omdat faculteiten heel groot kunnen worden...
fac :: Int -> Integer
fac n = product [1 .. fromIntegral n]

-- Hier moet de Integer weer geparst worden naar een Int...
catalan :: Int -> Int
catalan n = fromInteger $ div (fac (2 * n)) (fac (n + 1) * fac n)

-- 2
catalanRec :: Int -> Int
catalanRec 0 = 1 -- basisgeval
catalanRec npe =
  -- npe is (n+1)
  let n = npe - 1
   in div (2 * (2 * n + 1) * catalanRec n) (n + 2)

-- 3
--    implementatie 1, waarbij de formule herschreven werd met n als argument ipv (n+1)
catalanSum :: Int -> Int
catalanSum 0 = 1 -- basisgeval
catalanSum n = sum [catalanSum i * catalanSum (n - 1 - i) | i <- [0 .. n - 1]]

--    implemenatie 2, waarbij het argument (n+1) is, zoals in de opgave
catalanSum' :: Int -> Int
catalanSum' 0 = 1
catalanSum' npe =
  let n = npe - 1
   in sum [catalanSum' i * catalanSum' (n - i) | i <- [0 .. n]]

-- 4.
catalanAsy :: Int -> Double
catalanAsy n = fromIntegral (4 ^ n) / (fromIntegral n ** (3 / 2) * sqrt pi)

-- 5.
--    f is een functie met als domein de natuurlijke getallen
--    tabulatie zet de resultaten van deze functie, toegpast op de natuurlijke getallen, in een tabel
tabulatie :: (Int -> a) -> [a]
tabulatie f = map f [0 ..]

{- VB:
    ghci> take 10 $ tabulatie (*2)
    [0,2,4,6,8,10,12,14,16,18]
-}

-- lcatalan past de verschillende catalan functies toe
-- op de natuurlijke getallen aan de hand van tabulatie.

lcatalan :: [Int]
lcatalan = tabulatie catalan

lcatalanRec :: [Int]
lcatalanRec = tabulatie catalanRec

lcatalanSum :: [Int]
lcatalanSum = tabulatie catalanSum

-- 6.
odds :: [Int] -> [(Int, Int)]
odds c = filter (odd . snd) (zip [0 ..] c) -- snd is het catalangetal van het tupel, dat moet oneven zijn

-- als oneindige lijst...
oddsInf :: [(Int, Int)]
oddsInf = odds lcatalanRec

{- VB.
    ghci> take 6 $ oddsInf
    [(0,1),(1,1),(3,5),(7,429),(15,9694845),(31,14544636039226909)]
-}

-- 7.
testCatalan :: Int -> Int -> [(Bool, Double)]
testCatalan start einde =
  [(check n, error n) | n <- [start .. einde]]
  where
    -- check of catalan en catalanRec hetzelfde resultaat geven
    check :: Int -> Bool
    check n
      | catalan n == catalanRec n = True
      | otherwise = False
    -- bereken de relatieve fout van de asymptotische benadering
    error :: Int -> Double
    error n = (abs . (/ exact)) (catalanAsy n - exact)
      where
        exact = fromIntegral (catalanRec n)
