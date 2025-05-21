-- | Extraatje: factorisatie van positieve gehele getallen
primesUntil :: Int -> [Int]
primesUntil n = verwijderVeelvouden [2 .. n]
  where
    verwijderVeelvouden :: [Int] -> [Int]
    verwijderVeelvouden [] = error "Wordt niet gebruikt."
    verwijderVeelvouden (x : xs)
      | x * x > n = x : xs -- stop als sqrt(x) > n
      | otherwise = x : verwijderVeelvouden (filter (\e -> e `mod` x /= 0) xs)

{-
  factorenLijst::Int -> [Int] -> [Int]

  'factorenLijst x list'
  list is een lijst van priemgetallen niet groter dan x
  het resultaat is de lijst van priemfactoren van x
  waarbij een priemfactor even vaak voorkomt als het aantal
  keer dat hij x deelt
-}
factorenLijst :: Int -> [Int] -> [Int]
factorenLijst 1 _ = [] -- wat is het belang van deze lijn?
factorenLijst _ [] = []
factorenLijst x (y : list)
  | rem x y == 0 = y : factorenLijst (div x y) (y : list)
  | otherwise = factorenLijst x list

-- primes 12 = [2, 3, 5, 7, 11]
--
-- factorenLijst 12 [2, 3, 5, 7, 11]            12/2 = 6
-- = 2 : factorenLijst 6 [2, 3, 5, 7, 11]       6/2 = 3
-- = 2 : 2 : factorenLijst 3 [2, 3, 5, 7, 11]   2/3 \notin \N
-- = 2 : 2 : factorenLijst 3 [3, 5, 7, 11]      3/3 = 1
-- = 2 : 2 : 3 : factorenLijst 1 [3, 5, 7, 11]  basisgeval

{-
  factoren::Int -> [Int]

 'factoren x' is de lijst van priemdelers van x
-}
factoren :: Int -> [Int]
factoren x = factorenLijst' x (primesUntil x)

{-
ghci> factoren (maximum (primesUntil 1000000))
[999983]

ghci> factoren (maximum (primesUntil (maximum (primesUntil 10000) - 1))*maximum (primesUntil 10000))
[9967,9973]
-- Omdat beide getallen priem zijn, en hun product uniek is.
-}

factorenLijst' :: Int -> [Int] -> [Int]
factorenLijst' n (p : ps)
  | n `mod` p == 0 = p : factorenLijst (n `div` p) (p : ps) -- div voor delen van ints
  | otherwise = factorenLijst n ps