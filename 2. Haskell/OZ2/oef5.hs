{-
    5. Hamming Numbers
-}

-- 1. Merge

merge :: (Ord a) => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x : xs) (y : ys)
  | x > y = y : merge (x : xs) ys
  | x < y = x : merge xs (y : ys)
  | otherwise = x : merge xs ys -- x == y

-- 2. Hamming

-- a Hamming number is an integer of the form 2^i · 3^j · 5^k,
-- for non-negative integers i, j and k.
hamming :: [Integer]
hamming =
  1
    : merge
      (merge (map (* 2) hamming) (map (* 3) hamming))
      (map (* 5) hamming)