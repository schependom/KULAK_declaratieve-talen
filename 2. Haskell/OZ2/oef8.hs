{-
    8. List Comprehensions Without The Sugar
-}

-- rewrite list comprehensions using
--      -> concat
--      -> map
--      -> filter

lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p as = [f a | a <- as, p a]

-- f:   functie
-- p:   predicaat
-- as:  lijst van a's

lc1' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1' f p = map f . filter p -- eta gereduceerd

{-

ghci> lc1 (*2) odd [1..10]
[2,6,10,14,18]

ghci> lc1' (*2) odd [1..10]
[2,6,10,14,18]

-}

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = [f a b | a <- as, b <- bf a, p b]

-- f:   functie a->b->c
-- as:  lijst van a's
-- bf:  functie van a naar lijst van b's
-- p:   predicaat over b
-- output is lijst van c's

lc2' :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2' f as bf p = concat [map (f a) (filter p (bf a)) | a <- as]

lc2'' :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2'' f as bf p =
  concat $
    map
      ( \a ->
          map (f a) (filter p (bf a))
      )
      as

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n =
  [ f a b c
    | a <- [1 .. n],
      even a,
      b <- [a .. n],
      c <- [b .. n],
      a * a + b * b == c * c
  ]

lc3' :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3' f n =
  concatMap
    ( \a ->
        concatMap
          ( \b ->
              map (f a b) $
                filter
                  (\c -> a * a + b * b == c * c)
                  [b .. n]
          )
          [a .. n]
    )
    (filter even [1 .. n])