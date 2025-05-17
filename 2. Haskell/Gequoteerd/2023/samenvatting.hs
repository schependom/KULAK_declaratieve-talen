import Text.Read -- voor readMaybe

{-------------
EERSTE GETAL DAT AAN VOORWAARDE VOLDOET
--------------}

eersteGetal :: Int
eersteGetal = head [x | x <- [1 ..], voorwaarde x]
  where
    voorwaarde :: Int -> Bool
    voorwaarde x = x ^ 3 == 27

-- eerste getal x uit een lijst l dat aan voorwaarde (f x == True) voldoet
eersteGetalVw :: (Int -> Bool) -> [Int] -> Int
eersteGetalVw f l = head [x | x <- l, f x]

{-------------
IO
--------------}

progIO :: IO ()
progIO = do
  putStrLn "Dit is de query voor de user"
  s <- getLine
  let maybeInt = (readMaybe :: String -> Maybe Int) s
   in if maybeInt == Nothing
        then do
          -- do !
          putStrLn "Error! Geef een getal!!"
          progIO
        else
          -- convert Just Int to Int
          let Just int = maybeInt
           in do
                putStrLn "Dit is je getal plus een:"
                print $ succ int

-- nu iets properder

progIO' :: IO ()
progIO' = do
  putStrLn "Dit is de query voor de user"
  s <- getLine
  case (readMaybe s :: Maybe Int) of
    Nothing -> do
      putStrLn "Error! Geef een getal!!"
      progIO
    Just int -> do
      putStrLn "Dit is je getal plus een:"
      print (succ int)