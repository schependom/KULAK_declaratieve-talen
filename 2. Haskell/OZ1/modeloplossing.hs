{-
  Dit zijn oplossingen voor Haskell assignment1.
  Gebruik hlint om deze te evalueren en zie wat je
  daaruit kan leren. Dit bereidt al voor op wat
  nog komt.
-}
-- | 1 List Operations
{-
  Schrijf een functie mnteller::[Int] -> Int
  die het aantal elementen in een lijst telt
-}

mnteller::[a] -> Int
mnteller [] = 0
mnteller (x:xs) = 1 + mnteller xs


{-
   Schrijf een functie myAnd::[Bool] -> Bool
   die een lijst als input neemt en
   die True teruggeeft als alle elementen
   in de lijst True zijn en False in het andere
   geval
-}

myAnd::[Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

{-
   Schrijf een functi myOr::[Bool] -> Bool
   die een lijst als input neemt en
   die True teruggeeft als minstens 1 element
   in de lijst True is en False in het andere
   geval
-}

myOr::[Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

{-
   Schrijf een functie myAppend::[Int] -> [Int] -> Int
   die twee lijsten als input neemt en
   hun concatenatie berekent
-}
myAppend::[Int] -> [Int] -> [Int]
myAppend x [] = x
myAppend [] x = x
myAppend (x:xs) y = x:myAppend xs y

{-
   Schrijf een functie myProduct::[Integer] -> Integer
   die een lijst als input neemt en
   het product van de elementen in de lijst
   berekent
-}
myProduct:: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x*myProduct xs

{-
   Schrijf een functie myInsert::Int -> [Int] -> Int
   die een Int en een lijst van Int als input neemt en
   en die de Int invoegt in de lijst op de eerste plaats
   waar het kleiner dan of gelijk is aan het volgende 
   element
-}
myInsert::Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (xe:xs)
  | x > xe = xe:myInsert x xs
  | otherwise = x:xe:xs 

-- | 2 Abstract Data Types

{-
  2.1 Name, Pair, Gender, Person, TestResult
-}
data Name = String
  deriving Show
data Pair = P (Int, Int)
  deriving Show
data Gender = Male|Female|Other
  deriving (Show)
data Person = Pn Name Int Gender
  deriving Show
data TestResult =
  Pass Int 
  | Fail [String]
  deriving Show

{-
  2.1
  genderToString::Gender -> String
  stringToGender::String -> Gender

  zet een Gender om in een String en omgekeerd
-}
genderToString::Gender -> String
genderToString Male    = "male"
genderToString Female  = "female"
genderToString Other   = "other"

stringToGender::String -> Gender
stringToGender "male"     = Male
stringToGender "female"   = Female
stringToGender _          = Other

{-
  passing::Int -> TestResult -> Int

  Creeer een passing resultaat met de
  gegeven graad
-}
passing::Int -> TestResult
passing = Pass

{-
  failing::[String] -> TestResult

  Creeer een failing resultaat met de
  gegeven commentaren
-}
failing::[String] -> TestResult
failing = Fail 

{-
  grade::TestResult -> Int

  Geef de graad van het resultaat terug
  Een fail leidt tot een resultaat 0
-}
grade::TestResult -> Int
grade (Pass x) = x
grade (Fail _) = 0

{-
  comments::TestResult -> [String]

  Geef de commentaren bij een resultaat terug
  Een passing resultaat leidt tot geen commentaar
-}
comments::TestResult -> [String]
comments (Pass _) = []
comments (Fail comms) = comms

-- | 3 Rock Paper ..

{-
  De mogelijke zetten worden voorgesteld door ADT Move
-}
data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

{-
  beat::Move -> Move

  Voor een Move zet is 'beat zet' de zet die
  de gegeven zet verslaat
-}
beat::Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

{-
  lose::Move -> Move

  Voor een Move zet is 'lose zet' de zet die
  van de gegeven zet verliest
-}
lose::Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

{-
  ADT voor het resultaat van een spel
-}
data Result = Win | Lose | Draw
  deriving (Eq, Show)

{-
  outcome::Move->Move->Result
 
  Het resultaat van 1 gevecht
-}
outcome::Move->Move->Result
outcome m1 m2
  | beat m1 == m2 = Lose
  | beat m2 == m1 = Win
  | otherwise = Draw

-- 4 Lists

{-
  factorial::Integer->Integer

  'factorial n' is gelijk aan n faculteit voor Integer n >= 0
  Als n < 0, dan is het resultaat 1
-}
-- Gebruik makend van de ingebouwde functie product
facproduct::Integer -> Integer
facproduct n = product [1..n]
-- Gebruik makend van de ingebouwde functie foldl
facfoldl::Integer -> Integer
facfoldl n = foldl (*) 1 [1..n]
-- Gebruik makend van recursie en patternmatching
facrecursie::Integer -> Integer
facrecursie n
  | n <= 0    = 1
  | otherwise = n * facrecursie (n - 1)

{-
  myRepeat::Int -> Int -> [Int]

  'myRepeat n x' is een lijst van n keer
  de waarde x
-}
myRepeat::Int -> Int -> [Int]
myRepeat n x = [x| _ <- [1..n]]

{-
  flatten::[[Int]] -> [Int]

  flatten zet een lijst van lijsten om in
  de lijst van waarden in die lijsten
-}
flatten::[[Int]] -> [Int]
flatten l = [x|sl <- l, x <- sl]

{-

  range::Int -> Int -> [Int]

  'range b t' is een gesorteerde lijst van
  de gehele getallen x zodat b <= x <= t
-}
-- gebruik makend van de operator (..)
range::Int -> Int -> [Int]
range b t = [b..t]
-- gebruik makend van recursie
rangerecursie::Int -> Int -> [Int]
rangerecursie b t
  | t < b = []
  | otherwise = b:rangerecursie (b+1) t

{-
  removeMultiples::Int -> [Int] -> [Int]

  'removeMultiples waarde lijst' is de gegeven lijst
  waaruit alle veelvouden van waarde
  verwijderd zijn
-}
removeMultiples::Int -> [Int] -> [Int]
removeMultiples g l = [x|x <- l,x `mod` g /= 0]

{-
5 Extra: Arithmetics
-}

{-
  Een uitdrukking wordt voorgesteld door het ADT Exp
-}
data Exp = Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Eq, Show)

{-
  eval::Exp -> Int

  'eval' is een Interpreter. Deze evalueert een uitdrukking 
  zoals ze werd voorgesteld in de parameter
-}
eval::Exp -> Int
eval (Const x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

{-
  Een Compiler zet een uitdrukking om in een programma
  Een programma is een lijst van instructies
  Een instructie wordt voorgesteld door de ADT Inst
-}
data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)
-- Een programma
type Prog = [Inst]
-- Een stapel
type Stack = [Int]

-- Voer een instructie uit op een stapel
execute :: Inst -> Stack -> Stack
execute (IPush x) s = x:s
execute IAdd (x1:x2:xs) = (x1 + x2):xs
execute ISub (x1:x2:xs) = (x1 - x2):xs
execute IMul (x1:x2:xs) = (x1 * x2):xs
execute _ _ = runtimeError 

-- Voer een programma uit op een stapel
-- (iteratie van instructies)
run::Prog -> Stack -> Stack
run [] s = s
run (inst:ps) s  = run ps (execute inst s)

-- Zet een uitdrukking om in een programma
compile :: Exp -> Prog
compile (Const x)  = [IPush x]
compile (Add e1 e2) = compile e2 ++ compile e1 ++ [IAdd]
compile (Sub e1 e2) = compile e2 ++ compile e1 ++ [ISub]
compile (Mul e1 e2) = compile e2 ++ compile e1 ++ [IMul]

runtimeError :: Stack
runtimeError = error "Runtime error."

{-
  6 Approximating pi
-}

{-
  sumf::[Float] -> Float

  'sumf lijst' is de som van de gegeven lijst
   van reele getallen
-}
sumf::[Float] -> Float
sumf [] = 0
sumf (x:xs) = x + sumf xs

{-
  prodf::[Double] -> Double

  'prodf lijst' is het product van de gegeven lijst
   van reele getallen
-}
-- Experimenteer zelf met Float.
prodf::[Double] -> Double
prodf [] = 1
prodf (x:xs) = x * prodf xs

-- Bereken pi met de eerste benaderingsformule
piSum::Float -> Float
piSum n = 8*sumf [1/(4*x+1)/(4*x+3)|x <- takeWhile (<=n) [0..]]

-- Bereken pi met de tweede benaderingsformule
piProd::Double -> Double
piProd n = 4*prodf [(2*x + 2)*(2*x+4)/((2*x+3)**2)|x <- takeWhile (<=n) [0..]]
{-
Prime numbers
-}

{-
  sieve::Int->[Int] -> [Int]

  De zeef van eratosthenes op basis van removeMultiples
-}
sieve::Int->[Int] -> [Int]
sieve top [] = []
sieve top (x:xs)
  | x*x > top   = x:xs -- Dit is onze versie van 'x > sqrt(top)
  | otherwise    = x:sieve top (removeMultiples x xs)

primes::Int -> [Int]
primes top = sieve top [2..top]

-- | Extraatje: factorisatie van positieve gehele getallen
{-
  factorenLijst::Int -> [Int] -> [Int]

  'factorenLijst x list'
  list is een lijst van priemgetallen niet groter dan x
  het resultaat is de lijst van priemfactoren van x
  waarbij een priemfactor even vaak voorkomt als het aantal
  keer dat hij x deelt
-}
factorenLijst::Int -> [Int] -> [Int]
factorenLijst 1 _ = [] -- wat is het belang van deze lijn?
factorenLijst _ [] = []
factorenLijst x (y:list)
  | rem x y  == 0    = y:factorenLijst (div x y) (y:list)
  | otherwise        = factorenLijst x list

{-
  factoren::Int -> [Int]

 'factoren x' is de lijst van priemdelers van x  
-}
factoren::Int -> [Int]
factoren x = factorenLijst x (primes x)

{-
  probeer factoren (maximum (primes 1000000))
  probeer factoren (maximum(primes (maximum (primes 10000) - 1))*maximum (primes 10000))
  verklaar
-}
