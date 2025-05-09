{-
  foldr uit HC
-}

-- foldr      f           z    l    resultaat
foldr' :: (e -> r -> r) -> r -> [e] -> r
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

-- f is bij som (+) en bij prod (*)
-- PREFIX notatie van OPERATOREN!!

-- som
som :: [Int] -> Int
som l = foldr' (+) 0 l

-- som, eta gereduceerd!
som' :: [Int] -> Int
som' = foldr' (+) 0

-- product, eta gereduceerd!
prod :: [Int] -> Int
prod = foldr' (+) 0

-- bind definitie
ma >>= f = join (fmap f ma)

-- list monad
instance Monad [] where
  xs >>= f = concat (map f xs)

lijstMonad ::
  [Int]
    resultaat = [1, 2] >>= \x -> [x, x + 10]

-- results in [1,11,2,12]

-- maybe monad
instance Monad Maybe where
  Just x >>= f = f x
  Nothing >>= _ = Nothing