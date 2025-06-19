import Control.Monad
import Prelude

{-

Eenvoudig voorbeeld van een Monad.

De gegevensstructuur 'Cum' houdt bij hoe vaak een bind werd uitgevoerd
om het resultaat te berekenen.
- De eenvoudige functies plus1, telop en telop3 en priem gelden als voorbeeld.
- De functie readCum leest een Cum van stdio. (indien a van klasse Read is (niet gezien))
- De show van Cum toont enkel de waarde van het resultaat.
- De functie 'geschiedenis' toont het aantal binds.

We sluiten af met een meer gevorderd voorbeeld.

-}

-- =========================================================================
{-
  ! Om iets een Monad instantie te maken, moet het ook een instantie hebben van:
      1. Functor (logisch!), want [fmap :: a -> b -> f a -> f b] nodig.
      2. Applicative, want nodig:
          - pure  :: a -> appl a
          - <*>   :: appl (a -> b) -> appl a -> appl b

  (<*>) :: Applicative f => f (a -> b) -> f a -> f b

  VB1: Maybe
    Just (+3) <*> Just 4    -- Result: Just 7
    Nothing   <*> Just 4    -- Result: Nothing
    Just (+3) <*> Nothing   -- Result: Nothing

  VB2: List
    [(+1), (*2)] <*> [1,2]  -- Result: [2,3,2,4]
    -- Explanation: Applies each function to each value
    -- (+1) 1 = 2, (+1) 2 = 3, (*2) 1 = 2, (*2) 2 = 4

  VB3: IO
    main = do
      let action1 = return (+3)
      let action2 = return 4
      result <- action1 <*> action2
      print result
    -- Output: 7
-}

-- Voorwaarden om Cum een instantie van Monad te maken
instance Applicative Cum where
  -- pure: gelijkaardig aan return (gewoon schil rond zetten)
  pure :: a -> Cum a
  pure = C 0 -- dit kan weggelaten worden, de Monad moet dan return definieren (deprecated)

  -- <*> is sequentiele applicatie
  (<*>) :: Cum (a -> b) -> Cum a -> Cum b
  (<*>) = ap

instance Functor Cum where
  fmap :: (a -> b) -> Cum a -> Cum b
  -- liftM :: Monad m => (a -> b) -> m a -> m b
  -- liftM f m  = m >>= (return . f)
  --            = m >>= \x -> return (f x)
  fmap = liftM

-- =========================================================================

-- Int stelt het aantal binds voor, a is eender wat (polymorfisme)
data Cum a = C Int a

instance (Show a) => Show (Cum a) where
  show :: (Show a) => Cum a -> String
  show (C _ x) = show x -- toon enkel x, niet het aantal binds.

-- de teller wordt beschouwd als een zij-effect
-- de onderstaande definitie zorgt ervoor dat Cum a aan de Monad wetten kan voldoen
instance (Eq a) => Eq (Cum a) where
  (==) :: (Eq a) => Cum a -> Cum a -> Bool
  (==) (C _ x) (C _ y) = x == y

-- (/=) wordt automatisch geimplementeerd als volgt:
-- (/=) (C _ x) (C _ y) = not (x == y)

-- return het aantal binds (In t)
geschiedenis :: Cum a -> Int
geschiedenis (C h _) = h

instance Monad Cum where
  --  return :: a -> Cum a
  --  return = C 0            -- dit kan gebruikt worden ipv pure bij Applicative (deprecated)
  (>>=) :: Cum a -> (a -> Cum b) -> Cum b
  (>>=) (C b x) f =
    let C bf y = f x -- bf is het aantal binds van f
     in C (bf + b) y -- voeg het aantal binds van f en het huidige aantal binds toe

-- je moet (bf+b+1) doen als je het aantal binds wil tellen, maar
-- we hebben de implementatie aangepast zodat enkel het aantal
-- toegangen tot lijstelementen wordt geteld.

lees :: (Read a) => String -> Cum a
lees s = C 0 (read s)

leesIO :: IO (Cum Int)
leesIO = do
  s <- getLine -- teller niet omhoog, want bind van IO en niet van Cum
  -- let v = lees s :: Cum Int
  --  in return v
  return (lees s :: Cum Int) -- zet IO wrapper rond (Cum Int)

telopIO :: IO (Cum Int)
telopIO = do
  c1 <- leesIO -- c1 :: Cum Int
  c2 <- leesIO -- c2 :: Cum Int
  let r = telop c1 c2 -- telop :: Cum Int -> Cum Int -> Cum Int
  return r -- return :: Cum Int -> IO (Cum Int)

vertelover :: (Cum Int -> Cum Int -> Cum Int) -> IO String
vertelover f = do
  c1 <- leesIO -- c1 :: Cum Int
  c2 <- leesIO -- c2 :: Cum Int
  let r = f c1 c2 -- r :: Cum Int
  return ("resultaat : " ++ show r ++ ", geschiedenis : " ++ show (geschiedenis r))

{-
ghci> vertelover telopr
2
3
"resultaat : 5, geschiedenis : 3"

-}

plus1 :: Cum Int -> Cum Int
plus1 c = do
  v <- c -- teller plus 1, want we doen een bind op Cum Int, niet op IO (...)
  return (v + 1) -- return :: Int -> Cum Int

-- => teller is plus 1
{-
ghci> geschiedenis (plus1 (C 0 3))
1
-}

telop :: Cum Int -> Cum Int -> Cum Int
telop a b = do
  v1 <- a -- teller plus 1
  v2 <- b -- teller plus 1
  return (v1 + v2)

-- => teller is plus 2
{-
ghci> geschiedenis (telop (C 0 3) (C 0 4))
2
-}

telop3 :: Cum Int -> Cum Int -> Cum Int -> Cum Int
telop3 a b c = do
  v1 <- a -- teller plus 1
  v2 <- b -- teller plus 1
  v3 <- c -- teller plus 1
  return (v1 + v2 + v3)

-- => teller is plus 3

telop3r :: Cum Int -> Cum Int -> Cum Int -> Cum Int
telop3r a b c = do
  v <- telop a b -- telop geeft Cum Int terug (en heeft de teller met 2 verhoogd), de bind nu verhoogt de teller nog eens met 1
  v3 <- c -- teller plus 1
  return (v + v3)

-- => teller is plus 4
{-
ghci> geschiedenis (telop3r (C 0 3) (C 0 4) (C 0 5))
4
-}

telopr :: Cum Int -> Cum Int -> Cum Int
telopr a b = do
  v <- telop a b -- telop geeft Cum Int terug (en heeft de teller met 2 verhoogd), de bind nu verhoogt de teller nog eens met 1
  return v

-- => teller is plus 3!!
{-
ghci> geschiedenis (telopr (C 0 3) (C 0 4))
3
-}

prob :: Int -> Cum Int
prob y = do
  if y > 0
    then do
      a <- (C 0 3) -- 1 bind
      b <- (C 0 4) -- 2e bind
      return (a + b) -- teller is 2
    else return 0 -- teller is 0

{-
ghci> geschiedenis (prob 2)
2
ghci> show (prob 2)
"7"

ghci> geschiedenis (prob 0)
0
-}

{-
De functies hieronder zoeken een waarde in een stijgende lijst,
geven True terug als de waarde gevonden is en False indien niet,
Door het gebruik van Cum houden ze ook het totaal aantal operaties bij

  - vind          doorloopt de volledige lijst
  - vindstijgend  stopt als de waarde in de lijst groter is dan wat we zoeken
  - vindbs        gebruikt binary search

Dit wordt geillustreerd in
  test::Int->Int->[(Bool,Int)]
die op drie manieren een getal zoekt in een stijgende rij van drievouden,
het resultaat en het aantal binds rapporteert

Oef: pas de functies aan zodat enkel toegangen tot lijstelementen geteld worden

  => Hiervoor moet je (>>=) aanpassen, zodat het nieuwe aantal binds gelijk
     is aan het oude aantal binds + het aantal binds van de functie, maar niet
     sowieso plus 1 doen.
  => We voegen expliciet bij functies die toegang tot de lijst doen
     een bind toe, zodat we het aantal binds kunnen bijhouden.
     Dit kan aan de hand van [_ <- C 1 ()].
-}

-- Controleert of de Cum (Monad) van een lijst leeg is
leeg :: (Eq a) => Cum [a] -> Cum Bool
leeg ml = do
  l <- ml
  return $ null l

check :: (a -> a -> Bool) -> Cum a -> Cum a -> Cum Bool
check f mx my = do
  vx <- mx
  vy <- my
  return (f vx vy)

kop :: Cum [a] -> Cum a
kop ml = do
  l <- ml
  _ <- C 1 () -- toegang tot lijstelement -> verhoog de teller met 1
  return (head l)

staart :: Cum [a] -> Cum [a]
staart ml = do
  l <- ml
  _ <- C 1 () -- toegang tot lijstelement -> verhoog de teller met 1
  return (tail l)

-- Eq type constraint is nodig om de check functie te kunnen gebruiken
vind :: (Eq a) => Cum a -> Cum [a] -> Cum Bool
vind x ml = do
  lg <- leeg ml
  if lg
    then return False
    else do
      gelijkAanKop <- check (==) x (kop ml)
      if gelijkAanKop
        then return True
        else vind x (staart ml)

-- Ord nodig om de vergelijking te kunnen maken
-- [a] is een STIJGENDE lijst!!
vindstijgend :: (Ord a) => Cum a -> Cum [a] -> Cum Bool
vindstijgend x ml = do
  lg <- leeg ml
  if lg
    then return False
    else
      let head = kop ml
       in do
            groterDanHead <- check (>) head x
            if groterDanHead
              then return False
              else do
                gelijkAanHead <- check (==) x head
                if gelijkAanHead
                  then return True
                  else vindstijgend x (staart ml)

-- Pas functie toe op de lijst in de Cum
funlijst :: ([a] -> [b]) -> Cum [a] -> Cum [b]
funlijst f ml = do
  l <- ml
  return (f l)

-- Bereken op basis van de elementen in Cum [a] een b en zet het resultaat in een Cum b
evallijst :: ([a] -> b) -> Cum [a] -> Cum b
evallijst e ml = do
  l <- ml
  return (e l)

-- Pak het pde element uit de lijst in Cum [a]
pik :: Int -> Cum [a] -> Cum a
pik p ml = do
  l <- ml
  _ <- C 1 ()
  return (l !! p)

-- Vind met binary search, in plaats van ganse lijst (of op basis van stijgende volgorde)
vindbs :: (Eq a, Ord a) => Cum a -> Cum [a] -> Cum Bool
vindbs x l = do
  len <- evallijst length l
  if len == 0
    then return False -- niet gevonden
    else do
      let midden = len `div` 2 -- midden is de index van het midden element
          mid = pik midden l -- pik returnt een Cum a
      gelijkAanMidden <- check (==) x mid
      if gelijkAanMidden
        then return True
        else do
          groterDanMidden <- check (>) x mid
          if groterDanMidden
            then vindbs x (funlijst (drop (midden + 1)) l) -- funlijst past een functie toe op de lijst binnen de Cum schil
            else vindbs x (funlijst (take midden) l)

-- Vergelijk de 3 zoekmethoden
test :: (Ord p, Num p, Enum p) => p -> p -> [(Cum Bool, Int)]
test teZoeken aantalVeelvouden =
  let veelvoudenVan3 = [3, 6 .. 3 * aantalVeelvouden]
      gewoon = vind (C 0 teZoeken) (C 0 veelvoudenVan3)
      stopoptijd = vindstijgend (C 0 teZoeken) (C 0 veelvoudenVan3)
      binair = vindbs (C 0 teZoeken) (C 0 veelvoudenVan3)
   in [ (gewoon, geschiedenis gewoon),
        (stopoptijd, geschiedenis stopoptijd),
        (binair, geschiedenis binair)
      ]

-- Test op de Monad wetten
x5 :: Int -> Cum Int
x5 x = C 0 (x * 5)

-- x van tot
-- Doe begin en eind maal x en tel op
xvt :: (Num a) => a -> a -> a -> Cum a
xvt begin eind x = C 0 (sum [x * l | l <- [begin, eind]])

someC :: Cum Int
someC = C 0 (-3)

testLaws :: [Bool]
testLaws =
  [ -- voor L.U.L. (Left Unit Law XD), moet f::a->mb
    x5 101 == (return 101 >>= x5), -- left unit law:  (return x >>= f) == (f x)
    someC == (someC >>= return), -- right unit law:   (m >>= return) == m
    -- 2 checks...
    (someC >>= x5 >>= (xvt (-10) 3)) == (someC >>= (\x -> (x5 x) >>= (xvt (-10) 3))), -- associativity law
    (someC >>= (xvt (-10) 3) >>= (x5)) == (someC >>= (\x -> ((xvt (-10) 3 x) >>= (x5))))
  ]
