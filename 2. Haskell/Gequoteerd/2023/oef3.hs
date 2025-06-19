import Text.Read

tekenHor :: Int -> IO ()
tekenHor b = putStrLn $ concat $ replicate b "="

tekenBord :: Int -> Int -> [String] -> IO ()
tekenBord h b lijnen = do
  -- TEKEN HET BORD
  tekenHor b
  mapM_ putStrLn lijnen -- !! mapM_ !!
  tekenHor b
  -- VRAAG INPUT
  putStrLn "Tekenen (j/n)?"
  yesNo <- getLine
  -- geen [if not good] maar eerder [is wel good] en helemaal op het einde else
  if yesNo == "ja"
    then do
      hoogte <- haalGetal "Hoogte?" 0 (h - 1)
      breedte <- haalGetal "Breedte?" 0 (b - 1)
      let nieuweLijnen = zetPunt breedte hoogte lijnen
       in tekenBord h b nieuweLijnen
    else do
      putStrLn "Bye"
      return ()

-- return een getal tussen de grenzen [onder, boven]
haalGetal :: String -> Int -> Int -> IO Int
haalGetal s onder boven = do
  putStrLn s -- stel de vraag
  getalS <- getLine -- haal het getal als string
  let getalM = readMaybe getalS -- !! lees de string als getal -> Maybe !!
   in if getalM == Nothing
        then do
          putStrLn "Gelieve een geldig getal tussen de grenzen in te geven!"
          haalGetal s onder boven -- probeer opnieuw
        else
          -- unificeer getal met het waarde-gedeelde van de Just
          let Just getal = getalM
           in return getal

haalGetal' :: String -> Int -> Int -> IO Int
haalGetal' s onder boven = do
  putStrLn s -- prompt
  putStr "-> "
  s <- getLine -- input user
  case readMaybe s of -- !! [case readMaybe ... of ...]
    Nothing -> do
      putStrLn "Error! Geef een geldig getal binnen de grenzen in!"
      haalGetal' s onder boven -- probeer opnieuw
    Just getal -> return getal

-- krijgt een valid input
zetPunt :: Int -> Int -> [String] -> [String]
zetPunt x y l =
  let rijenVoor = take y l
      rij = l !! y
      rijenNa = drop (y + 1) l
   in rijenVoor ++ [take x rij ++ "." ++ drop (x + 1) rij] ++ rijenNa

teken :: Int -> Int -> IO ()
teken h b = do
  -- teken het initiële bord
  -- roep tekenBord aan met h lijnen van b keer " "
  --
  -- waarbij replicate n == take n $ repeat
  -- want repeat geeft een oneindige lijst
  tekenBord h b (replicate h (concat $ replicate b " "))