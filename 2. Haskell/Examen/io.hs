import Control.Monad
import Text.Read

{-------------
CASE READMAYBE
--------------}

progIO' :: IO ()
progIO' = do
  putStrLn "Dit is de query voor de user"
  s <- getLine
  case (readMaybe s :: Maybe Int) of
    Nothing -> do
      putStrLn "Error! Geef een getal!!"
      progIO'
    Just int -> do
      putStrLn "Dit is je getal plus een:"
      print (succ int)
      putStrLn "Kunnen we hier verder gaan?"
      putStrLn "Jazeker!"

-- ! take 5 $ repeat "iets" == replicate 5 "iets"

prog2 :: IO ()
prog2 = do
  mapM_ putStrLn ["Vincent", "Is", "Een", "Tank"]
  -- ! Right operator section, want het rechterargument is gegeven!!
  mapM_ (putStrLn . (++ ". ")) ["Met", "Een", "Punt"]
  replicateM_ 3 (putStrLn "Dit wordt 3 keer gedrukt")
  print 3 -- werkt voor alles met Show instantie
  s <- getLine
  i <- readLn :: IO Int
  case readMaybe s of
    Nothing -> putStrLn "s kan niet omgezet worden naar een int."
    Just si -> putStrLn ("de integer die je ingaf is" ++ (show :: Int -> String) si)

{-------------
MAPM_
--------------}

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

haalGetal _ _ _ = return 0

zetPunt _ _ _ = ["Punt"]