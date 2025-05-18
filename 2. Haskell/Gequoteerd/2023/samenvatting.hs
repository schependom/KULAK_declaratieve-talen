-- voor readMaybe

import Data.List
import Data.Maybe
import Text.Read

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

{-------------
x `elem` l checkt of x een element is van de lijst (of string!) l!!
--------------}

countStringsThatEndInDigits :: [String] -> Int
countStringsThatEndInDigits lijst = length $ filter endsInDigit lijst
  where
    endsInDigit :: String -> Bool
    endsInDigit s = last s `elem` "012"

endsInOneOfDigits :: String -> String -> Bool
endsInOneOfDigits s d = last s `elem` d

{-------------
findIndex en elemIndex uit Array.List
--------------}

-- Char met ''
-- String met ""

rhymes :: [String] -> [String] -> Bool
rhymes l1 l2 =
  drop i1 l1 == drop i2 l2
  where
    i1 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") l1 -- FIND INDEX
    i2 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") l2

rhymes' :: [String] -> [String] -> Bool
rhymes' l1 l2 =
  drop i1 l1 == drop i2 l2
  where
    i1 = fromMaybe 0 $ elemIndex "0" l1 -- ELEM INDEX
    i2 = fromMaybe 0 $ elemIndex "0" l2

{-------------
all & alle paren genereren van een lijst adhv list comprehensions
--------------}

-- controleer of voor elk paar (x,y) geldt dat x GEEN prefix is van y
noPrefix :: (Eq a) => [[a]] -> Bool
noPrefix xs = all (\(x, y) -> not (isPrefix x y)) [(x, y) | x <- xs, y <- xs, x /= y]

-- is lijst1 de prefix van lijst2?
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix l1 l2 = take (length l1) l2 == l1

{-------------
GEEN 'IN' NODIG BIJ 'LET'

EN

NAAM GEVEN MET @NAAM
--------------}

run' :: IO ()
run' = do
  content <- readFile "input.txt"
  let entries = map parseEntry (lines content) -- !! geen 'in' nodig!!
  putStrLn "What word do you want to rhyme with?"
  word <- getLine

  case findWord word entries of
    Nothing -> putStrLn "I couldn't find the word"
    -- !! je kan de entry een naam geven!!
    Just entry@(MkEntry _ kind phonemes variations) -> do
      putStrLn $ word ++ " has " ++ show (countSyllables entry) ++ " syllable(s)"
      putStrLn "I found the following rhyme words:"

      let rhymingWords = filter (`rhymes` entry) entries
      mapM_ (\(MkEntry w _ _ _) -> putStr (w ++ " ")) rhymingWords

      putStrLn "\nHere are some sentences you can make using these words:"
      mapM_ putStrLn (take 10 (makeSentence rhymingWords))