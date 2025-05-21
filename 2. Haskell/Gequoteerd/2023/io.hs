import Control.Monad
import Text.Read

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

-- take 5 $ repeat "iets" == replicate 5 "iets"

prog2 :: IO ()
prog2 = do
  mapM_ putStrLn ["Vincent", "Is", "Een", "Tank"]
  mapM_ (putStrLn . (++ ". ")) ["Met", "Een", "Punt"]
  replicateM_ 3 (putStrLn "Dit wordt 3 keer gedrukt")
  print 3 -- werkt voor alles met Show instantie
  s <- getLine
  i <- readLn :: IO Int
  case readMaybe s of
    Nothing -> putStrLn "s kan niet omgezet worden naar een int."
    Just si -> putStrLn ("de integer die je ingaf is" ++ (show :: Int -> String) si)