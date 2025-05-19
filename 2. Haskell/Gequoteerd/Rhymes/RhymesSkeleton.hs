module RhymesSkeleton where

import Data.Char (toLower, toUpper)
import Data.List (find, findIndex, intercalate)
import Data.Maybe

---OPGAVE 1
data WordKind = Adjective | Noun | Verb deriving (Show, Eq) -- default structurele EQ is OK

data Entry
  = MkEntry
      String -- woord
      WordKind -- soort woord
      [String] -- fonemen
      [String] -- vervoegingen/verbuigingen
  deriving (Eq) -- automatisch gegenereerde Eq-instantie

k :: Entry
k = MkEntry "Kristien" Verb ["K", "R", "IE1", "S", "T", "IE2", "N"] ["Kristienen", "Kristienneke", "Kristatis", "KRISTIEEEEENNNN!!"]

instance Show Entry where
  show :: Entry -> String
  show (MkEntry w k f v) = map toUpper w ++ " " ++ intercalate "-" f ++ " " ++ take 1 (show k) ++ " " ++ intercalate " " v

---OPGAVE 2

splitBy' :: Char -> String -> [String]
splitBy' c l = reverse (go "" l [])
  where
    go :: String -> String -> [String] -> [String]
    go s [] l = s : l
    go s (x : xs) l
      | x == c = go "" xs (s : l)
      | otherwise = go (s ++ [x]) xs l

{-
ghci> splitBy '|' "vinc|ent|is|cool"
["vinc","ent","is","cool"]
-}

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy c (x : xs)
  | x == c = "" : rest -- start een nieuwe string vooraan de lijst
  | otherwise = (x : head rest) : tail rest -- voeg toe aan de string vooraan de lijst
  where
    rest = splitBy' c xs

parseEntry :: String -> Entry
parseEntry s =
  let parts = splitBy' ' ' s
      w = head parts
      f = splitBy' '-' (parts !! 1)
      k = case parts !! 2 of
        "A" -> Adjective
        "N" -> Noun
        "V" -> Verb
      v = splitBy' ' ' (last parts)
   in MkEntry w k f v

-- OPGAVE 3
countSyllables' :: Entry -> Int
countSyllables' (MkEntry _ _ f _) = go f 0
  where
    go :: [String] -> Int -> Int
    go [] acc = acc
    go (x : xs) acc =
      let l = last x
       in if l == '0' || l == '1' || l == '2' then go xs (succ acc) else go xs acc

endsInOneOfDigits :: String -> String -> Bool
endsInOneOfDigits s d = last s `elem` d

countSyllables :: Entry -> Int
countSyllables (MkEntry _ _ f _) = length $ filter (`endsInOneOfDigits` "012") f

-- OPGAVE 4
rhymes :: Entry -> Entry -> Bool
rhymes (MkEntry _ _ f1 _) (MkEntry _ _ f2 _) =
  drop i1 f1 == drop i2 f2
  where
    i1 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") f1
    i2 = fromMaybe 0 $ findIndex (`endsInOneOfDigits` "1") f2

-- OPGAVE 5
-- in dit geval is a == Char en onderzoeken we of een String een prefix is
-- van een andere String (beiden een lijst van Chars)

-- controleer of voor elk paar (x,y) geldt dat x GEEN prefix is van y
noPrefix :: (Eq a) => [[a]] -> Bool
noPrefix xs = all (\(x, y) -> not (isPrefix x y)) [(x, y) | x <- xs, y <- xs, x /= y]

-- is lijst1 de prefix van lijst2?
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix l1 l2 = take (length l1) l2 == l1

-- OPGAVE 6
makeSentence :: [Entry] -> [String]
makeSentence e =
  [ map toLower ("the " ++ a ++ " " ++ n ++ " " ++ last v3 ++ " " ++ a2) -- laatste vervoeging
    | (MkEntry a k1 _ _) <- e,
      k1 == Adjective,
      (MkEntry n k2 _ _) <- e,
      k2 == Noun,
      (MkEntry _ k3 _ v3) <- e,
      k3 == Verb,
      (MkEntry a2 k4 _ _) <- e,
      k4 == Adjective,
      a < a2, -- lexicografisch kleiner
      noPrefix [a, last v3, a2] -- geen prefixen
  ]

-- OPGAVE 7

findWord :: String -> [Entry] -> Maybe Entry
findWord s e = go e
  where
    go :: [Entry] -> Maybe Entry
    go [] = Nothing
    go ((MkEntry w k f v) : xs)
      | map toLower w == map toLower s = Just $ MkEntry w k f v
      | otherwise = go xs

findWord' :: String -> [Entry] -> Maybe Entry
findWord' s = find (\(MkEntry w _ _ _) -> map toLower w == map toLower s)

run :: IO ()
run = do
  f <- readFile "input.txt"
  let e = map parseEntry (lines f)
   in do
        putStrLn "What word do you want to rhyme with?"
        w <- getLine
        let entryM = findWord w e
         in if isJust entryM
              then
                -- unificeer
                let Just (MkEntry s k f v) = entryM
                 in do
                      putStrLn (w ++ " has " ++ show (countSyllables (MkEntry s k f v)) ++ " syllable(s)")
                      putStrLn "I found the following rhyme words:"
                      let rijmwoorden = [kand | kand <- e, rhymes kand (MkEntry s k f v)]
                       in do
                            mapM_ putStr [s ++ " " | (MkEntry s _ _ _) <- rijmwoorden]
                            putStrLn ""
                            putStrLn "Here are some sentences you can make using these words:"
                            mapM_ putStrLn (take 10 (makeSentence rijmwoorden))
              else do
                putStrLn "I couldn't find the word"
                return () -- stop

-- cleaner
run' :: IO ()
run' = do
  content <- readFile "input.txt"
  let entries = map parseEntry (lines content) -- !! geen 'in' nodig!!
  putStrLn "What word do you want to rhyme with?"
  word <- getLine

  case findWord word entries of
    Nothing -> putStrLn "I couldn't find the word"
    -- !! je kan de entry hernoemen!!
    Just entry@(MkEntry _ kind phonemes variations) -> do
      putStrLn $ word ++ " has " ++ show (countSyllables entry) ++ " syllable(s)"
      putStrLn "I found the following rhyme words:"

      let rhymingWords = filter (`rhymes` entry) entries
      mapM_ (\(MkEntry w _ _ _) -> putStr (w ++ " ")) rhymingWords

      putStrLn "\nHere are some sentences you can make using these words:"
      mapM_ putStrLn (take 10 (makeSentence rhymingWords))