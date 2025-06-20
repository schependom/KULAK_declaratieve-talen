import Control.Monad
import Data.Bits
import Data.Complex
import Data.Int
import Data.List

-- data Nullen
--   = N [Double]
--   | C [Complex Double]

-- t is ofwel Double ofwel (Complex Double)

-- te weinig tijd om dit polymorfisme te implementeren...
-- dus ik heb het enkel voor Double gedaan.
data Nullen = N [Double]

data Polynoom = P [Double]

-- =======
-- OEFENING 2.2
-- =======

instance Show (Polynoom) where
  show :: (Polynoom) -> String
  show (P coefLijst) =
    let graad = length coefLijst - 1
     in go graad coefLijst
    where
      go :: Int -> [Double] -> String
      go _ [] = []
      go gr (x : xs) =
        let graadString
              | gr == 0 = ""
              | gr == 1 = "x"
              | otherwise = "x^" ++ show gr
            teken
              | x >= 0 = "+"
              | otherwise = ""
         in teken ++ " " ++ show x ++ graadString ++ go (gr - 1) xs

{-
ghci> polynoom (N [1.0,2.0])
+ 1.0x^2 -3.0x+ 2.0
-}

-- =======
-- OEFENING 2.1
-- =======

-- de lijst coefficenten gaat van hoogste naar laagste graad
polynoom :: Nullen -> Polynoom
-- polynoom (N l) = if length l > 3 then error "Graad is te hoog"
polynoom (N [a]) = P [1, -a]
polynoom (N [a, b]) = P [1, -(a + b), a * b]
polynoom (N [a, b, c]) = P [1, -(a + b + b), a * b + a * c + b * c, a * b * c]

-- =======
-- OEFENING 2.3.1
-- =======

randomAdd :: (RandomGen g) => [Int] -> State g [Int]
randomAdd list = do
  n <- modify next
  return (n : list)

radd4assoc :: (RandomGen g) => State g [Int]
radd4assoc = randomAdd [] >>= (\s -> randomAdd s >>= (\sNew -> randomAdd sNew >>= randomAdd))

-- =======
-- OEFENING 2.3.2
-- =======

{-
e is het type van de elementen in de lijst

voor SP s a geldt dat
    - s = [LijstActie e]
    - a = [e]

SP s a is een state passing functie s -> (a,s)
-}

-- lijstacties bijhouden, M is Min, T is Toevoegen
data LijstActie e
  = M e
  | T e
  deriving (Show)

type StateLijst e = [LijstActie e]

-- livingPlus zet een State monad rond een state-passing functie SP (StateLijst a) a
-- State s a = State SP (s a)
-- met SP s a :: s -> (a,s)
livingListPlusNoMonad :: e -> [e] -> SP (StateLijst e) [e]
livingListPlusNoMonad getal lijst state =
  let newList = (getal : lijst)
      newState = (T getal : state)
   in (newList, newState)

-- opdat we kunnen verwijderen moeten we kunnen vergelijken (Eq!)
livingListMinNoMonad :: (Eq e) => e -> [e] -> SP (StateLijst e) [e]
livingListMinNoMonad _ [] currState = ([], currState)
livingListMinNoMonad getal (x : xs) currState
  | getal == x =
      let (nextList, nextState) = livingListMinNoMonad getal xs currState
       in (nextList, M getal : nextState)
  | otherwise =
      let (nextList, nextState) = livingListMinNoMonad getal xs currState
       in (x : nextList, nextState)

-- livingListPlus zet Monad schil (State) rond livingListPlusNoMonad
livingListPlus :: e -> [e] -> State (StateLijst e) [e]
livingListPlus getal lijst = modify $ livingListPlusNoMonad getal lijst

-- livingListMin doet hetzelfde, maar dan voor livingListMinNoMonad
livingListMin :: (Eq e) => e -> [e] -> State (StateLijst e) [e]
livingListMin getal lijst = modify $ livingListMinNoMonad getal lijst

klinkers :: [Char]
klinkers = ['a', 'e', 'i', 'o', 'u', 'y']

-- doe de klinkers weg
-- nu is het type van de elementen in de lijst gelijk aan e == Char
doeDeKlinkersWeg :: String -> State (StateLijst Char) [Char]
doeDeKlinkersWeg woord = doeDeKlinkersWegRec woord klinkers

-- recursieve versie die alle klinkers overloop
doeDeKlinkersWegRec :: String -> [Char] -> State (StateLijst Char) [Char]
doeDeKlinkersWegRec woord [] = do return woord
doeDeKlinkersWegRec woord (k : ks) = do
  eenKlinkerMinder <- livingListMin k woord
  doeDeKlinkersWegRec eenKlinkerMinder ks

{-
ghci> fst $ runState (doeDeKlinkersWeg "Dit willen we vandaag over Haskell horen.") []
"Dt wlln w vndg vr Hskll hrn."

ghci> snd $ runState (doeDeKlinkersWeg "Dit willen we vandaag over Haskell horen.") []
[M 'o',M 'o',M 'i',M 'i',M 'e',M 'e',M 'e',M 'e',M 'e',M 'a',M 'a',M 'a',M 'a']
-}

-- === Code paragraaf "7.4.3 The State Monad" in "bookHaskellTS.pdf" ===
type SP s a = s -> (a, s)

data State s a = State (SP s a)

bindSP :: SP s a -> (a -> SP s b) -> SP s b
bindSP m f = \s0 ->
  ( let (x, s1) = m s0
     in f x s1
  )

pureSP :: a -> SP s a
pureSP x = \s -> (x, s)

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure x = State (pureSP x)
  (<*>) = ap

instance Monad (State s) where
  m >>= f = State (bindSP (runState m) (runState . f))

runState :: State s a -> SP s a
runState (State m) = m

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s' = State (\s -> ((), s'))

modify :: (s -> (a, s)) -> State s a
modify f = State f

-- =======================================================
-- Eigen implementatie van een random-generator
-- zie System.Random voor een Haskell standaard

class RandomGen g where
  next :: g -> (Int, g)

-- random number generator (Marsaglia, George (July 2003). "Xorshift RNGs")
xsl x v = x `xor` (shiftL x v)

xsr x v = x `xor` (shiftR x v)

xorShift x = xsl (xsr (xsl x 13) 7) 17

data R = R Int deriving (Show)

instance RandomGen R where
  next :: R -> (Int, R)
  next (R x) = (mod x 256, R (xorShift x))

-- --------------------------------------
randomList :: (RandomGen g) => Integer -> State g [Int]
randomList n
  | n > 0 = do
      x <- modify next
      xs <- randomList (n - 1)
      return (x : xs)
  | otherwise = return []

{-
-- voorbeeld: rl is een State
-- runState rl (R 17) laat deze lopen met initiele waarde 17 voor de randomgenerator
-- het resultaat is een koppel bestaande uit de lijst van random getallen en de nieuwe toestand van de randomgenerator.

ghci> rl = randomList 7
ghci> fst $ runState rl (R 17)
[17,81,89,185,52,48,86]
ghci> snd $ runState rl (R 17)
R 5741632773585689292
ghci>
-}
