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

-- 2.
speelCantor :: Int -> IO ()
speelCantor n = do
  askInput n
  where
    -- gebruiker mag k keer een ongeldige invoer ingeven
    askInput :: Int -> IO ()
    askInput k = do
      putStrLn "Diepte:"
      s <- getLine -- string van user
      case (readMaybe :: String -> Maybe Int) s of
        Nothing ->
          if k > 1
            then do
              putStrLn $ "Ongeldige diepte! Je hebt nog " ++ show (k - 1) ++ " kans(en)..."
              askInput (k - 1) -- probeer opnieuw (met een beurt minder)
            else do
              putStrLn "Teveel fouten!"
              putStrLn "Gestopt"
              return ()
        Just i -> do
          -- getal is goed geparst
          putStrLn $ cantor i
          putStrLn "Nog eens? (j/n)"
          jaNeen <- getLine
          if jaNeen == "j"
            then speelCantor n -- reset de teller zodat je opnieuw n kansen hebt
            else do
              putStrLn "Stoppen..."
              return ()