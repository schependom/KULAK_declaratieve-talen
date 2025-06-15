import Control.Monad
import Data.Maybe
import Text.Read

uiteinde :: String
uiteinde = "x.x"

midden :: String
midden = "..."

vervang :: String -> String
vervang [] = ""
vervang (x : xs)
  | x == 'x' = uiteinde ++ vervang xs
  | otherwise = midden ++ vervang xs

-- 1.
cantor :: Int -> String
cantor 0 = "x"
cantor n = vervang $ cantor (n - 1)

{-
n=0     1 = 3^0
n=1     1 2 3 = 3^1
n=2     1 2 3 . . . 7 8 9 = 3^2
n=3     1 2 3 . . . 7 8 9 . . . . . . . . . 19 20 21 . . . 25  26 27 = 3^3
-}

cantor' :: Int -> String
cantor' n
  | n == 0 = "x"
  | otherwise =
      let prev = cantor $ pred n
       in concat
            [ prev,
              replicate (3 ^ (n - 1)) '.',
              prev
            ]

-- 2.
speelCantor :: Int -> IO ()
speelCantor n = do
  askInput "Diepte: " n
  where
    -- gebruiker mag k keer een ongeldige invoer ingeven
    askInput :: String -> Int -> IO ()
    askInput q k = do
      putStrLn q
      s <- getLine -- string van user
      case (readMaybe :: String -> Maybe Int) s of
        Nothing ->
          if k > 1
            then
              askInput (q ++ "??") (k - 1) -- probeer opnieuw (met een beurt minder)
            else do
              putStrLn "Teveel fouten!"
              putStrLn "Gestopt."
        Just i -> do
          -- getal is goed geparst
          putStrLn $ cantor i
          putStrLn "Nog eens? (j/n)"
          jaNeen <- getLine
          if jaNeen == "j"
            then speelCantor n -- reset de teller zodat je opnieuw n kansen hebt
            else putStrLn "Bye"

------------------
--- EXTRA

logBaseInt :: Int -> Int -> Int
logBaseInt b x = go 1 0
  where
    go acc counter
      | (acc * b) <= x = go (acc * b) (succ counter)
      | otherwise = succ counter

{-
b = 2, x = 3, counter=0
  1*2 <=  3 => acc = 1*2 = 2, counter=1
  2*2 >   3 => counter+1!!
-}

-- bereken hoe groot n moet zijn om minstens (positie) p lang te zijn
calcN :: Int -> Int
calcN = ceiling . log3 -- rond (log3 p) af naar boven

log3 :: Int -> Double
log3 n = logBase 3 (fromIntegral n)

-- nu met logBaseInt
calcN' :: Int -> Int
calcN' = logBaseInt 3

bezoekDeCantor :: IO ()
bezoekDeCantor = do
  putStrLn "Op welke plaats wil je beginnen (ik kies dan iets dat net groot genoeg is) : "
  g <- zekerPosInt' "?"
  putStrLn ("Dit is jouw cantorgetal [log_3(" ++ show g ++ ") naar boven afgerond]:")
  let cantorNummer = calcN' g
  print cantorNummer
  putStrLn ("Ik plaats je op positie " ++ show g ++ " van cantor " ++ show cantorNummer ++ "...")
  showZoom g (cantor cantorNummer)

-- showZoom neemt twee argumenten:
--    1.  pos :: Int
--        positie in de cantor
--    2.  fullCantor :: String
--        De volledige cantor, degene met
--        minimale grootte waarin de oorspronkelijke
--        positie aanwezig is
showZoom :: Int -> String -> IO ()
showZoom pos fullCantor = do
  let l = length fullCantor
  putStrLn $
    concat
      [ "[",
        show pos,
        ",",
        endPos pos l,
        "]",
        " van ",
        show l,
        ": ",
        take 100 $ drop pos fullCantor
      ]
  putStrLn "WAT WIL JE DOEN?"
  putStrLn "  l(inks)/r(echts)/p(ositie)/h(ou op)"
  actie <- getChar
  if actie /= 'h'
    then do
      newPos <- case actie of
        'r' -> return $ min (l - 10) (pos + 10)
        'l' -> return $ max 0 (pos - 10)
        'p' -> zekerPosInt' "-> Welke positie?"
        _ -> return pos
      putStr "-> "
      showZoom newPos fullCantor
    else putStrLn "\nBye"
  where
    endPos :: Int -> Int -> String
    endPos pos l
      | pos + 100 <= l = show $ pos + 100
      | otherwise = show l

zekerPosInt :: String -> IO Int
zekerPosInt query = do
  putStr $ query ++ " "
  s <- getLine
  let m = (readMaybe :: String -> Maybe Int) s
  case m of
    Nothing -> zekerPosInt $ query ++ "?"
    Just i -> if i >= 0 then return i else zekerPosInt $ query ++ "?"

zekerPosInt' :: String -> IO Int
zekerPosInt' query = do
  putStr $ query ++ " "
  s <- getLine -- string
  let m = readMaybe s -- Nothing of Just Int
  maybe
    (zekerPosInt' $ query ++ "?")
    (\i -> if i >= 0 then return i else zekerPosInt' $ query ++ "?")
    m

-- maybe neemt 3 argumenten:
--    1. default waarde als m = Nothing
--    2. functie met argument x waarbij m=(Just x)
--    3. Maybe