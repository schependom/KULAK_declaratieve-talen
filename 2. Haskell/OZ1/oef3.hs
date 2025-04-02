{-
    3. Rock, Paper, Scissors
-}

------
-- 3.1
------

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

-- dit is de pattern match approach,
-- maar kan ook met guard

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

------
-- 3.2
------

data Result
  = Win
  | Lose
  | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
  | beat m1 == m2 = Lose
  | lose m1 == m2 = Win
  | otherwise = Draw