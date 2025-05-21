{-
    5. A special request (EXTRA)
-}

-- PREREQUISITES
inc :: (Num a, Show a) => a -> IO a
inc x = do
  let y = x + 1
  print y -- print werkt voor instances van Show
  return y -- zet monad shell rond y::Int => (IO Int)

dec :: (Num b, Show b) => b -> IO b
dec x = do
  let y = x - 1
  print y
  return y

header :: IO ()
header = do
  putStrLn "----"
  putStrLn "Menu"
  putStrLn "----"
  putStrLn ""

listItems :: [(String, a -> IO a)] -> IO ()
listItems l = go l 1
  where
    go [] i = return () -- zelfde als do return ()
    go (x : xs) i = do
      putStrLn (show i ++ ". " ++ fst x)
      go xs (i + 1)

-- INPUTS
--    lijst met paren (beschrijving, actie)
--    initiele waarde
menu :: [(String, a -> IO a)] -> a -> IO a
menu l init = do
  header
  listItems l
  putStr "==> "
  i <- readLn -- readLn is hetzelfde als eerst getLine (:: IO String) en daarna read (String -> a) op gebinde waarde
  snd (l !! (i - 1)) init -- pas de (a -> IO a) functie toe op de initiele waarde

-- !!(0-based indexing!)