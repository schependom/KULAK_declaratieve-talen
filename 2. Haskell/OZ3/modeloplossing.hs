import Control.Monad
import Data.Char
import Data.List
import System.Random

-- Expressions
{-
  Beschouw de volgende ADT
-}
data MyBool = MyTrue | MyFalse

data Exp
  = Const MyBool
  | And Exp Exp
  | Or Exp Exp

{-
  1. Schrijf een instantie van de Eq klasse voor MyBool en Exp
     Geen evaluatie voor de uitdrukkingen is gevraagd, enkel
     letterlijke overeenkomst. Gebruik niet meer dan 4 gevallen
     voor (==)
-}

instance Eq MyBool where
  (==) MyTrue MyTrue = True
  (==) MyFalse MyFalse = True
  (==) _ _ = False

instance Eq Exp where
  (==) (Const a) (Const b) = a == b
  (==) (And e1 e2) (And f1 f2) = (e1 == f1) && (e2 == f2)
  (==) (Or e1 e2) (Or f1 f2) = (e1 == f1) && (e2 == f2)
  (==) _ _ = False

{-
  2. Schrijf een instantie van Show voor MyBool en Exp.
     De show methode zet een argument in een string om.
     Trek je niks aan van haakjes.
-}

instance Show MyBool where
  show MyTrue = "MnWaarheid"
  show MyFalse = "MnLeugen"

instance Show Exp where
  show (Const a) = show a
  show (And e1 e2) = show e1 ++ " && " ++ show e2
  show (Or e1 e2) = show e1 ++ " || " ++ show e2

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval MyTrue = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval (Const a) = eval a
  eval (And e1 e2) = eval e1 && eval e2
  eval (Or e1 e2) = eval e1 || eval e2

-- Sequences
{-
  Definieer een type classe Sequence a die bestaat uit de functies next en prev
  om naar de vorige en de volgende elementen in de sequentie te vragen.
-}

class Sequence a where
  next :: a -> a
  prev :: a -> a

instance Sequence Integer where
  next x = x + 1
  prev x = x - 1

instance Sequence Char where
  next x
    | x < 'a' || x > 'z' = error ("character " ++ [x] ++ " out of range")
    | x == 'z' = error "no character after z"
    | otherwise = chr (ord x + 1)
  prev x
    | x < 'a' || x > 'z' = error ("character " ++ [x] ++ " out of range")
    | x == 'a' = error "no character before a"
    | otherwise = chr (ord x - 1)

instance Sequence Bool where
  next True = False
  next False = True
  prev = Main.next

class (Sequence a) => LeftBoundedSequence a where
  firstElement :: a

class (Sequence a) => RightBoundedSequence a where
  lastElement :: a

instance LeftBoundedSequence Bool where
  firstElement = False

instance RightBoundedSequence Bool where
  lastElement = True

instance LeftBoundedSequence Char where
  firstElement = 'a'

instance RightBoundedSequence Char where
  lastElement = 'z'

{-
  Drilling on IO
-}
{-
  1. Shrijf een programma prog1::IO ()
     dat twee natuurlijke getallen me en n leest
     en dan m keer n op de output afdrukt
-}
-- Eerste oplossing
prog1 :: IO ()
prog1 = do
  m <- readLn
  n <- readLn
  zet m n
  where
    zet :: Int -> Int -> IO ()
    zet m n =
      if m == 0
        then return ()
        else do
          print n
          zet (m - 1) n

-- Tweede oplossing (let op de type-annotatie bij het lezen van n)
prog1_2 :: IO ()
prog1_2 = do
  m <- readLn
  n <- readLn :: IO Integer
  mapM_ print (take m (repeat n))

-- Derde oplossing (het is irrelevant wat het type van n is)
prog1_3 :: IO ()
prog1_3 = do
  m <- readLn
  n <- getLine
  mapM_ putStrLn (take m (repeat n))

-- Dit kan ook als volgt:
prog1_4 :: IO ()
prog1_4 = do
  m <- readLn
  n <- readLn :: IO Integer
  mapM_ print (replicate m n)

-- Overigens...
prog1_5 :: IO ()
prog1_5 = do
  m <- readLn
  n <- readLn :: IO Integer
  replicateM_ m (print n)

{-
  2. Schrijf prog1b gebruik makend van lambda expressies
-}
prog1b :: IO ()
prog1b = readLn >>= \m -> readLn >>= \n -> zet m n

zet :: Int -> Int -> IO ()
zet m n = if m == 0 then return () else print n >> zet (m - 1) n

{-
  3. Schrijf een programma dat lijnen blijft lezen en ze omgekeerd afdrukt
     tot een blanco lijn wordt gelezen
-}
prog2 = do
  l <- getLine
  if l /= ""
    then do
      putStrLn (reverse l)
      prog2
    else return ()

{-
  4. Schrijf een functie index die een lijst indexed in een IO kader
-}
index :: [IO a] -> IO Int -> IO a
index l f = do
  i <- f
  l !! i

{-
  4 EXTRA Recursie: een oneindige lus
-}
loop :: a -> (a -> IO a) -> IO a
loop n pa = do
  y <- pa n
  loop y pa

{-
  5. WEL kies uit een menu wat met de input moet gebeuren
-}
-- Deze versie geeft geen informatie terug
-- Gebruik:
-- > menu [("Omhoog",inc),("Omlaag",dec)] 18
menu :: [(String, a -> IO a)] -> a -> IO ()
menu m r = do
  putStrLn "____________________"
  putStrLn "Menu"
  putStrLn "--------------------"
  doMenu 1 m
  putStr "==> "
  li <- readLn
  let l = li - 1
  if l < length m
    then do
      snd (m !! l) r
      return ()
    else menu m r
  return ()

doMenu :: Int -> [(String, a -> IO a)] -> IO ()
doMenu _ [] = putStrLn "______________"
doMenu p (x : xs) = do
  putStrLn (show p ++ ". " ++ fst x)
  doMenu (p + 1) xs

-- Een versie van menu die wel informatie teruggeeft
-- Gebruik bijvoorbeeld:
-- > resultaat = menu2 [("Omhoog",inc),("Omlaag",dec)] 18
-- > do {l <- resultaat;if (l > 18) then return "Waw" else return "Ooh"}

menu2 :: [(String, a -> IO a)] -> a -> IO a
menu2 m r = do
  putStrLn "____________________"
  putStrLn "Menu"
  putStrLn "--------------------"
  doMenu 1 m
  putStr "==> "
  li <- readLn
  let l = li - 1
  if l < length m
    then snd (m !! l) r
    else menu2 m r

-- voor de demo
inc x = do
  let y = x + 1
  print y
  return y

dec x = do
  let y = x - 1
  print y
  return y

{-
  6 Mooie bomen
-}
data Tree a = Node a (Tree a) (Tree a) | Nil

instance (Show a) => Show (Tree a) where
  show t = concat (intersperse "\n" (maakLijnen t "" ""))

maakLijnen :: (Show a) => Tree a -> String -> String -> [String]
maakLijnen Nil _ _ = []
maakLijnen (Node x l r) iacc pref =
  [iacc ++ pref ++ show x]
    ++ maakLijnen l (iacc ++ blanc pref) "|-- "
    ++ maakLijnen r (iacc ++ blanc pref) "'-- "
  where
    blanc :: String -> String
    blanc "" = ""
    blanc "|-- " = "|   "
    blanc "'-- " = "    "

-- Voor de tests, maak een random boom met n knopen
-- gebruik g = mkStdGen 16041956 (of een ander getal) om een generator aan te maken
-- (r,ng) = randomR (x,y) g gebruikt dan deze generator g
-- om een random waarde r in het interval [x,y]
-- en een nieuwe generator ng te produceren
-- merk op dat maakBoom een zuivere functie is
maakBoom :: (RandomGen g) => Int -> g -> Tree Int
maakBoom 0 _ = Nil
maakBoom n g =
  let (v, gt) = randomR (0, n - 1) g
      (gl, gr) = split gt
   in Node n (maakBoom v gl) (maakBoom (n - v - 1) gr)

{-
  7 Voeg een element toe aan een boom, maar houd hem in evenwicht
-}
addToTree :: Tree a -> a -> Tree a
addToTree Nil x = Node x Nil Nil
addToTree (Node v l r) x = Node v r (addToTree l x)

{-
  8 Schrijf een programma dat in een oneindige loop de gebruiker toelaat
    om een nieuw element toe te voegen of om de boom af te drukken.
-}
-- behandel neemt als parameter een boom van elementen die kunnen gelezen worden met readLn
-- roep bijvoorbeeld op met 'behandel (NIL::Tree Int)'
-- Merk op dat we menu2 gebruiken, die een waarde teruggeeft
behandel :: (Show a, Read a) => Tree a -> IO ()
behandel mnBoom = do
  nwBoom <-
    menu2
      [ ( "Voegtoe",
          \b -> do
            putStr "> "
            v <- readLn
            return (addToTree b v)
        ),
        ("ToonBoom", \b -> print b >> return b)
      ]
      mnBoom
  behandel nwBoom

{-
  9 We kunnen het niet laten
-}
-- menu2 toont geen foutboodschap als er een verkeerd nummer wordt ingegeven,
-- maar laat gewoon onmiddellijk het menu terug verschijnen

menu3 :: [(String, a -> IO a)] -> a -> IO a
menu3 m r = do
  putStrLn "____________________"
  putStrLn "Menu"
  putStrLn "--------------------"
  doMenu 1 m
  putStr "==> "
  li <- readLn
  let l = li - 1
  if l < length m
    then snd (m !! l) r
    else do
      print "???"
      menu3 m r

-- looop doen stoppen is echt niet moeilijk
-- probeer looop 10 inc 10
-- wat zie je?
-- indien niks, probeer dan looop 10 inc 0
looop :: a -> (a -> IO a) -> Int -> IO a
looop n _ 0 = return n
looop n pa h = do
  y <- pa n
  looop y pa (h - 1)

-- behandelMeer laat toe om de som van de elementen in de boom te tonen
behandelMeer :: (Show a, Read a, Num a) => Tree a -> IO ()
behandelMeer mnBoom = do
  nwBoom <-
    menu3
      [ ( "Voeg toe",
          \b -> do
            putStr "> "
            v <- readLn
            return (addToTree b v)
        ),
        ("Toon Boom", \b -> print b >> return b),
        ( "Toon Som",
          \b ->
            print (somVanBoom b)
              >> return b
        )
      ]
      mnBoom
  behandelMeer nwBoom

somVanBoom :: (Num a) => Tree a -> a
somVanBoom Nil = 0
somVanBoom (Node v l r) = v + (somVanBoom l) + (somVanBoom r)
