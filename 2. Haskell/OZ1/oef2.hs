{-
  2. Algebraic Datatypes = ADTs
-}

data Name = MkName String
  deriving (Show) -- niet vergeten!

-- afspraak: dataCONSTRUCTOR verschillende naam (e.g. Mk...) van dataTYPE!

myName :: Name -- niet MkName!! Want het type is Name
myName = MkName "Vincent"

------- SEMANTIEK --------
-- Name is het dataTYPE
-- MkName is de dataCONSTRUCTOR
-- String is een PARAMETER of een FIELD

data Pair = MkPair Int Int
  deriving (Show)

-- (product) datatype pair
-- dataconstructor MkPair
-- 2 parameters/velden Int Int

myPair :: Pair
myPair = MkPair 69 420

-- som datatype (unie):
data Gender = Male | Female | Other
  deriving (Show, Eq) -- +Eq!! anders geen vergelijkingen

data Gender' = Male' | Female' | Other'

-- expliciet (als we bijvoorbeeld geen automatische structurele gelijkheid willen infereren)
instance Eq Gender' where
  -- minimale implementatie is (==) of (/=)
  -- deze twee operatoren zijn elkaars complement
  (==) Male' Male' = True
  (==) Female' Female' = True
  (==) Other' Other' = True
  (==) _ _ = False

myGender = Male -- automatische typeinferentie

data Person = MkPerson Name Int Gender

-- custom Show instantie van de klasse Show
-- waar
--
--  class Show a where
--    show :: a -> String
--  (Show :: *->*)
--
-- het kind van Show is *->* (recursief geval)
--
-- is gedefinieerd in de standaard library
instance Show Person where
  show :: Person -> String
  show (MkPerson (MkName n) i g) =
    "Name: " ++ n ++ ", Age: " ++ show i ++ ", Gender: " ++ show g

myPerson :: Person
myPerson = MkPerson (MkName "vincent") 19 Male

-- combinatie somtype en producttype:
data TestResult
  = Pass Int -- grade
  | Fail [String] -- comments from teacher
  deriving (Show)

myGoodTestResult = Pass 18

myBodTestResult = Fail ["Didn't study enough"]

stringToGender :: String -> Gender
stringToGender s -- guard:
  | s == "Male" = Male
  | s == "Female" = Female
  | otherwise = Other

genderToString :: Gender -> String
genderToString g -- opnieuw een guard:
  | g == Male = "Male"
  | g == Female = "Female"
  | otherwise = "Other"

passing :: Int -> TestResult
passing = Pass -- na Eta reductie!, was passing g = Pass g

failing :: [String] -> TestResult
failing = Fail -- na Eta reductie!

grade :: TestResult -> Int
grade (Pass g) = g -- grade
grade (Fail _) = 0 -- zero for fail

{-
!! NIET Pass of Fail maar passing of failing !
Die maken namelijk een TestResult aan!

ghci> grade (passing 20)
20

ghci> grade (failing ["Bad Score"])
0
-}

comments :: TestResult -> [String]
comments (Fail c) = c -- comments
comments (Pass _) = [] -- no comments (empty list)
