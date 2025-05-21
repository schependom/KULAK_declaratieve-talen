{-
    7. Prime Numbers (EXTRA)
-}

sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor

sieve :: Int -> [Int]
sieve n = verwijderVeelvouden [2 .. n]
  where
    verwijderVeelvouden :: [Int] -> [Int]
    verwijderVeelvouden [] = error "Wordt niet gebruikt."
    verwijderVeelvouden (x : xs)
      | x > floorMono (sqrtMono $ i2d n) = x : xs
      | otherwise = x : verwijderVeelvouden (filter (\e -> e `mod` x /= 0) xs)

sieve' :: Int -> [Int]
sieve' n = verwijderVeelvouden [2 .. n]
  where
    verwijderVeelvouden :: [Int] -> [Int]
    verwijderVeelvouden [] = error "Wordt niet gebruikt."
    verwijderVeelvouden (x : xs)
      | x * x > n = x : xs -- stop als sqrt(x) > n
      | otherwise = x : verwijderVeelvouden (filter (\e -> e `mod` x /= 0) xs)

primes :: [Int]
primes = verwijderVeelvouden [2 ..]
  where
    verwijderVeelvouden :: [Int] -> [Int]
    verwijderVeelvouden [] = error "Wordt niet gebruikt."
    verwijderVeelvouden (x : xs) = x : verwijderVeelvouden (filter (\e -> e `mod` x /= 0) xs)