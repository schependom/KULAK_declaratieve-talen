{-
    3. List Comprehensions
-}

mapLC :: (a -> b) -> [a] -> [b]
mapLC f l = [f k | k <- l]

{-
    mapLC (+1) [1,2,3]
    -> [2,3,4]
-}

filterLC :: (a -> Bool) -> [a] -> [a]
-- filterLC f l = [k | k <- l, f k == True]
filterLC f l = [k | k <- l, f k]

-- odd :: Int -> Bool
-- odd x = x `mod` 2 == 0
-- odd = not . even
-- Of met RIGHT OPERATOR SECTION
--      odd x = (`mod` 2) x == 0

{-
    filterLC odd [1,2,3,4,5]
    [1,3,5]
-}