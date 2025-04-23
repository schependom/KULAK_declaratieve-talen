import Data.Char

{-
    2. Sequences
-}

class Sequence a where
  next :: a -> a
  prev :: a -> a

instance Sequence Integer where
  next :: Integer -> Integer
  next = succ
  prev :: Integer -> Integer
  prev i = i - 1

geldig :: Char -> Bool
geldig z = isLetter z && isLower z

instance Sequence Char where
  next :: Char -> Char
  next x
    | geldig $ chr $ succ $ ord x = chr $ succ $ ord x
    | otherwise = error ("No value after '" ++ [x] ++ "'")
  prev :: Char -> Char
  prev 'a' = error "No value before 'a'"
  prev z = chr $ pred $ ord z

instance Sequence Bool where
  next :: Bool -> Bool
  next = not -- Next False = True en Next True = False
  prev :: Bool -> Bool
  prev = next -- Prev False = Next False = True

-- subclasses of type class Sequence
--
--      via =>
--      Superclass => Subclass
--      for every Subclass, there exists a Superclass
--      Dus de pijl staat in feite in de omgekeerde richting

class (Sequence a) => LeftBoundedSequence a where
  firstElem :: a

class (Sequence a) => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem :: Char
  firstElem = 'a'

instance RightBoundedSequence Char where
  lastElem :: Char
  lastElem = 'z'

instance LeftBoundedSequence Bool where
  firstElem :: Bool
  firstElem = False

instance RightBoundedSequence Bool where
  lastElem :: Bool
  lastElem = True