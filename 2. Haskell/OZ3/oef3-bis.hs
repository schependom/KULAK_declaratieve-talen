import Control.Monad

{-
    3. Drilling IO Bis
-}

myGetLine :: IO String -- IO plan dat een string returnt
myGetLine = do
  l <- getLine
  return l

myGetLine' :: IO String
myGetLine' = do
  getLine

myGetLine'' :: IO String
myGetLine'' = getLine -- geen do voor single statement

------------------------------------------------------------------

prog1 :: IO ()
prog1 = do
  mS <- getLine -- IO String naar String
  let m = (read mS :: Int) -- !!  (1) ::Int nodig!   (2) get IO (..) dus geen <-
  nS <- getLine
  let a = replicate m (putStrLn nS) -- stel lijst van IO actions op
  sequence a -- voer lijst van IO actions uit -> resultaat is een IO [()]
  return () -- dicard het resultaat => gebruik hiervoor beter sequence_ zoals hieronder

prog1' :: IO ()
prog1' = do
  m <- readLn -- !! combinatie van read en getLine !!
  nS <- getLine
  sequence_ $ replicate m (putStrLn nS) -- voer lijst van IO () acties uit -- sequence waarbij resultaat gediscard wordt

prog1'' :: IO ()
prog1'' = do
  m <- readLn -- Int
  nS <- getLine -- String
  replicateM_ m (putStrLn nS) -- replicateM waarbij resultaat gediscard wordt

prog1''' :: IO ()
prog1''' =
  do
    m <- readLn -- Int
    n <- readLn :: IO Int -- Int -> heeft een Show instantie -> print werkt met printStrLn niet
    zet m n
  where
    zet left x
      | left == 0 = return ()
      | otherwise = do
          print x -- print kan enkel als x een Show instantie heeft
          zet (left - 1) x

------------------------------------------------------------------

-- zet apart
zet :: Int -> String -> IO ()
zet left x =
  if left == 0
    then return ()
    else putStrLn x >> zet (left - 1) x -- resultaat moet niet doorgespeeld worden

zet' :: (Show a) => Int -> a -> IO ()
zet' left x =
  if left == 0
    then return ()
    else print x >> zet' (left - 1) x -- resultaat moet niet doorgespeeld worden

zet'' :: Int -> String -> IO ()
zet'' m n = replicateM_ m (putStrLn n) -- discard het resultaat

zet''' :: Int -> String -> IO ()
zet''' m n = sequence_ $ replicate m (putStrLn n) -- explicieter

prog1b :: IO ()
prog1b =
  readLn >>= \m ->
    getLine >>= \n ->
      zet m n

prog1b' :: IO ()
prog1b' =
  readLn >>= \m ->
    (readLn :: IO Int) >>= \n ->
      zet' m n

------------------------------------------------------------------

prog2 :: IO ()
prog2 = do
  s <- getLine
  if s == ""
    then return ()
    else do
      putStrLn $ reverse s
      prog2'

prog2' :: IO ()
prog2' = do
  s <- getLine
  unless
    (s == "")
    ( do
        putStrLn $ reverse s
        prog2'
    )

------------------------------------------------------------------

index :: [IO a] -> IO Int -> IO a
index l i =
  do
    getal <- i
    l !! getal