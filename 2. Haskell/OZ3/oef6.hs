{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use intercalate" #-}
import Data.List

{-
    5. Bomen die gezien mogen worden (EXTRA)
-}

{-
    INTERSPERSE:

    ghci> intersperse "\n" ["dit is een lijn", "dit is nog een lijn"]
    ["dit is een lijn","\n","dit is nog een lijn"]

    IN COMBINATIE MET CONCAT:

    ghci> concat (intersperse "\n" ["dit is een lijn", "dit is nog een lijn"])
    "dit is een lijn\ndit is nog een lijn"
-}

data Tree a
  = Node a (Tree a) (Tree a)
  | Nil

instance (Show a) => Show (Tree a) where
  show :: Tree a -> String
  show t = concat $ intersperse "\n" $ genLines t "" ""

niveauOnder :: String -> String
niveauOnder "" = ""
niveauOnder "|-- " = "|   "
niveauOnder "'-- " = "    "

genLines :: (Show a) => Tree a -> String -> String -> [String]
genLines Nil _ _ = []
genLines (Node v l Nil) acc prefix =
  (acc ++ prefix ++ show v)
    : genLines l (acc ++ niveauOnder prefix) "'-- "
genLines (Node v l r) acc prefix =
  (acc ++ prefix ++ show v)
    : genLines l (acc ++ niveauOnder prefix) "|-- "
    ++ genLines r (acc ++ niveauOnder prefix) "'-- "