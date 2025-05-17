module RhymesSkeleton where

import Data.Char (toLower, toUpper)

---OPGAVE 1
data WordKind = DummyWordKind

data Entry = DummyEntry deriving (Eq, Show)

---OPGAVE 2
parseEntry :: String -> Entry
parseEntry = undefined

-- OPGAVE 3
countSyllables :: Entry -> Int
countSyllables = undefined

-- OPGAVE 4
rhymes :: Entry -> Entry -> Bool
rhymes = undefined

-- OPGAVE 5
noPrefix :: (Eq a) => [[a]] -> Bool
noPrefix = undefined

-- OPGAVE 6
makeSentence :: [Entry] -> [String]
makeSentence = undefined

-- OPGAVE 7
run :: IO ()
run = undefined
