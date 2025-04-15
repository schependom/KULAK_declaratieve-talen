{-
    2. Function Chaining
-}

-- input
--      functie van a naar b
--      functie van b naar c
--      waarde van a
-- output
--      waarde van c
dot :: (b -> c) -> (a -> b) -> a -> c
dot g f x = g (f x)

dollar :: (a -> b) -> a -> b
dollar f x = f x

-- argumenten
--      lijst van functies van a naar a
--      waarde van a
-- output
--      waarde van a
applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f : fs) x = applyAll fs . f $ x

-- met foldl
--
--      applyAll [f,g,h] x = f . g . h $ x
--      applyAll [f,g,h] = f . g . h = ((id . f) . g) . h
--
-- h l = foldl f z l
--      z = id
--      f = (.)
applyAllF :: [a -> a] -> a -> a
applyAllF l x = foldl (.) id l x

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = applyAll [f | _ <- [1 .. n]]

-- dit is een eta gereduceerde versie van
--      applyTimes n f x = applyAll [ f | _ <- [1..n] ] x
-- merk op dat [1..0] = []!!
-- applyTimes 3 (+1) 0 = applyAll [ (+1), (+1), (+1) ] 0

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x functies = map ($ x) functies

{-
    applyMultipleFuncs 2 [(*2), (*3), (+6)]
    = map ($ 2) [(*2), (*3), (+6)]
    = [(*2) 2, (*3) 2, (+6) 2]
    = [4, 6, 8]
-}

-- (* 2) is partial application:
--      (*) x y = x * y
--      (*) x = \y -> x * y

-- ($ x) is een RIGHT OPERATOR SECTION
--      -> We geven het rechter argument (x) mee
--      -> Het linkerargument (de functie) niet
--
--  bv. positive :: Int -> Bool
--      positive = (> 0)
--      positive 5 = True

-- Anderzijds is er ook een LEFT OPERATOR SECTION
--  bv. positive :: Int -> Bool
--      positive = (< 0)
--      positive 5 = False

-- Dit is PARTIAL FUNCTION APPLICATION

-- Voor add geldt dat
--      add :: Int -> Int -> Int
--      add x = \y -> x + y
-- gelijk is aan
--      add :: Int -> (Int -> Int)
--      add' x y = x + y
-- Dit is een CURRIED FUNCTION

-- We kunnen nieuwe functies maken die afgeleid zijn van anderen
-- Zie bv.
--      inc = add 1
-- Dit is gelijk aan
--      inc' = \x -> add 1 x

add x = \y -> x + y

add' x y = x + y

inc = add 1

inc' = \x -> add 1 x