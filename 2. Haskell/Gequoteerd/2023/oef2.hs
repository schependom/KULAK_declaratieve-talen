{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- 1
nondiv :: Int -> [Int] -> Bool
nondiv _ [] = True
nondiv n (x : xs)
  | n `mod` x == 0 = False
  | otherwise = nondiv n xs

-- 2
priem_r_t_r_a :: Int -> Bool
priem_r_t_r_a x = (x > 1) && nondiv x [2 .. x - 1] -- groter dan 1!

-- eerste getal x uit een lijst l dat aan voorwaarde (f x == True) voldoet
eersteGetalVw :: (Int -> Bool) -> [Int] -> Int
eersteGetalVw f l = head [x | x <- l, f x]

-- 3
priem_f :: Int -> [Int]
priem_f n
  | n == 1 = [2] -- 2 is het eerste priemgetal.
  | otherwise =
      let vorigePriemgetallen = priem_f (n - 1)
          laatstePriemgetal = last vorigePriemgetallen
       in vorigePriemgetallen
            ++ [ eersteGetalVw -- zoek het eerste getal dat aan een voorwaarde voldoet
                   ( -- voorwaarde voor kandidaat x:
                     --    er zijn geen priemgetallen in vorigePriemgetallen die een deler
                     --    zijn van x en waarvan het kwadraat kleiner of gelijk is aan x
                     \x -> nondiv x (takeWhile (\p -> p ^ 2 <= x) vorigePriemgetallen)
                   )
                   -- het volgende priemgetal is groter dan het laatste!
                   [laatstePriemgetal + 1 ..]
               ]

-- 4. recursief gedefinieerde lijst
priem :: [Int]
priem = 2 : [k | k <- [3 ..], nondiv k (takeWhile (\p -> p ^ 2 <= k) priem)]

-- ALTERNATIEF

-- Verwijder veelvouden
vv :: Int -> [Int] -> [Int]
vv n = filter ((/= 0) . (`mod` n))

priem' :: [Int]
priem' = go [2 ..]
  where
    go (x : xs) = x : go (vv x xs)
