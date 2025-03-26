import System.Posix (DL (Null))

data Name = MkName String
  deriving (Show)

myName = MkName "Vincent"

-- Name is het dataTYPE
-- MkName is de dataCONSTRUCTOR
-- String is een argument type

data Pair = MkPair Int Int
  deriving (Show)

myPair = MkPair 69 420

data Gender = Male | Female | Other
  deriving (Show, Eq) -- anders geen vergelijkingen

myGender = Male

data Person = MkPerson Name Int Gender
  deriving (Show)

myPerson = MkPerson (MkName "vincent") 19 Male

data TestResult
  = Pass Int -- grade
  | Fail [String] -- comments from teacher
  deriving (Show)

myGoodTestResult = Pass 18

myBodTestResult = Fail ["Didn't study enough"]

stringToGender :: String -> Gender
stringToGender s
  | s == "Male" = Male
  | s == "Female" = Female
  | otherwise = Other

genderToString :: Gender -> String
genderToString g
  | g == Male = "Male"
  | g == Female = "Female"
  | otherwise = "Other"

passing :: Int -> TestResult
passing = Pass -- na Eta reductie!

failing :: [String] -> TestResult
failing = Fail -- na Eta reductie!

grade :: TestResult -> Int
grade (Pass g) = g -- grade
grade (Fail _) = 0 -- zero for fail

{-
niet Pass of Fail maar passing of failing!!

ghci> grade (passing 20)
20

ghci> grade (failing ["Bad Score"])
0
-}

comments :: TestResult -> [String]
comments (Fail c) = c -- comments
comments (Pass _) = [] -- no comments (empty list)
