{-
    3. Drilling on IO
-}

{-
    1. Let op indentatie in do-blok

    2. Type van do-blok is gelijk aan het type van diens laatste expressie

    3. Elke lijn in do-blok: waarde van type (IO a)
            var <- return (normal call)
            let var = normal call

    4. Equivalente uitdrukkingen

            do  l <- getLine
                return l

            do getLine

            getLine

    5. Type () = "unit":

            data () = ()

        No useful result in IO-actions
-}

{-
    DOCUMENTATIE
        - ...index.html
        - Prelude
        - Data.List
        - Control.Monad
-}

-- 1.
prog1 :: IO ()
prog1 = do
  m <- getLine
  n <- getLine
  -- toon m keer n, elk op een aparte lijn
  putStrLn $ concat $ replicate (read m :: Int) (n ++ "\n")

-- 2.
-- Zelfde programma, maar nu met bind operator (>>=)
prog1b :: IO ()
prog1b =
  getLine >>= \m ->
    getLine >>= \n ->
      -- toon m keer n, elk op een aparte lijn
      putStrLn $ concat $ replicate (read m :: Int) (n ++ "\n")

{-
  Klopt wegens equivalentie van

  do  x1 <- iets
      x2 <- f x1

      en

  iets >>= \x1 ->
    f x1 >>= \x2 ->
      ...
-}

-- 3.

-- output omgekeerde string tot lege string ingelezen
prog2 :: IO ()
prog2 = do
  l1 <- getLine
  if l1 /= ""
    then do
      putStrLn $ reverse l1
      prog2
    else return () -- stop

-- prog2b, nu met bind
prog2b :: IO ()
prog2b =
  getLine >>= \l1 ->
    if l1 /= ""
      then do
        putStrLn $ reverse l1
        prog2b
      else return () -- stop

-- 4.
index :: [IO a] -> IO Int -> IO a
index list ioAction =
  do
    i <- ioAction
    if i < 0 || i >= length list
      then error "Index out of bounds"
      else list !! i -- !! is de index operator