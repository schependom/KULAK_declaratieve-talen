import Control.Monad
import Prelude

{-
Eenvoudig voorbeeld van een Monad.

? De gegevensstructuur 'Cum' houdt bij hoe vaak een bind werd uitgevoerd
? om het resultaat te berekenen.

De eenvoudige functies plus1, telop en telop3 en priem gelden als voorbeeld.
De functie readCum leest een Cum van stdio. (indien a van klasse Read is (niet gezien))
De show van Cum toont enkel de waarde van het resultaat.
De functie 'geschiedenis' toont het aantal binds.

We sluiten af met een meer gevorderd voorbeeld
-}
-- =========================================================================
-- Voorwaarden om Cum een instantie van Monad te maken
instance Applicative Cum where
  pure :: a -> Cum a
  pure = C 0 -- dit kan weggelaten worden, de Monad moet dan return definieren (deprecated)

  (<*>) :: Cum (a -> b) -> Cum a -> Cum b
  (<*>) = ap

{-
The <*> operator allows you to apply functions inside
the Cum context to values inside the same context,
while accumulating the "history" (the number of binds).
-}

{-

!! DEFINITIE ap:
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return (f x)

!! DUS: TWEE BINDS!!

ap mf mx =
  !mf >>= \f ->
    !mx >>= \x ->
      return (f x)
-}

instance Functor Cum where
  fmap :: (a -> b) -> Cum a -> Cum b
  fmap = liftM

-- =========================================================================

-- Definieer de Algebraic Datatype (ADT)
--    met TYPEPARAMETER a
--    en een Int als enige PARAMETER/FIELD
data Cum a = C Int a

-- Maak een Show instantie aan
--    de show functie roept gewoon de show aan van de
--    ! TYPE PARAMETER a !
instance (Show a) => Show (Cum a) where
  show :: Cum a -> String
  show (C _ x) = show x

geschiedenis :: Cum a -> Int
geschiedenis (C h _) = h

instance Monad Cum where
  return :: a -> Cum a
  return = C 0 -- dit kan gebruikt worden ipv pure bij Applicative (deprecated)

  (>>=) :: Cum a -> (a -> Cum b) -> Cum b
  (C count1 x) >>= f =
    let C count2 y = f x
     in C (count1 + count2 + 1) y

lees :: (Read a) => String -> Cum a
lees s = C 0 (read s)

leesIO :: IO (Cum Int)
leesIO = do
  s <- getLine
  let parsed = lees s -- zet de teller op nul
   in return parsed

{-
ghci> leesIO
vincent
\*** Exception: Prelude.read: no parse

ghci> leesIO
23
23
-}

telopIO :: IO (Cum Int)
telopIO = do
  c1 <- leesIO -- de <- is equivalent met [leesIO >>= \c1 -> (...)] ==> IO schil verwijderd ==> c1 is (Cum Int)!
  c2 <- leesIO
  let r = telop c1 c2
   in return r -- zet IO schil rond resultaat van type (Cum Int)

vertelover :: (Cum Int -> Cum Int -> Cum Int) -> IO String
vertelover f = do
  c1 <- leesIO -- count op 0
  c2 <- leesIO -- count op 0
  let r = f c1 c2 -- count wordt ??
   in return ("resultaat : " ++ show r ++ ", geschiedenis : " ++ show (geschiedenis r))

vertelover' :: (Cum Int -> Cum Int -> Cum Int) -> IO String
vertelover' f =
  leesIO >>= \c1 ->
    leesIO >>= \c2 ->
      let r = f c1 c2
       in return ("resultaat : " ++ show r ++ ", geschiedenis : " ++ show (geschiedenis r))

plus1 :: Cum Int -> Cum Int
plus1 c = do
  i <- c -- verwijder de Cum schil => v is een getal
  return (i + 1)

telop :: Cum Int -> Cum Int -> Cum Int
telop a b = do
  i1 <- a
  i2 <- b
  return (i1 + i2)

telop3 :: Cum Int -> Cum Int -> Cum Int -> Cum Int
telop3 a b c = do
  i1 <- a
  i2 <- b
  i3 <- c
  return (i1 + i2 + i3)

telop3r :: Cum Int -> Cum Int -> Cum Int -> Cum Int
telop3r a b c = do
  i <- telop a b
  i' <- c
  return (i + i')

telopr :: Cum Int -> Cum Int -> Cum Int
telopr a b = do
  i <- telop a b
  return i

prob y =
  if y > 0
    then do
      a <- C 0 3
      b <- C 0 4
      return (a + b)
    else return 0

{-
De functies hieronder zoeken een waarde in een stijgende lijst,
geven True terug als de waarde gevonden is en False indien niet,
Door het gebruik van Cum houden ze ook het totaal aantal operaties bij

vind doorloopt de volledige lijst
vindstijgend stopt als de waarde in de lijst groter is dan wat we zoeken
vindbs gebruikt binary search

Dit wordt geillustreerd in
test::Int->Int->[(Bool,Int)]
die op drie manieren een getal zoekt in een stijgende rij van drievouden,
het resultaat en het aantal binds rapporteert

Oef: pas de functies aan zodat enkel toegangen tot lijstelementen geteld worden
-}

leeg :: (Eq a) => Cum [a] -> Cum Bool
leeg l = do
  vl <- l
  return (vl == [])

check :: (a -> a -> Bool) -> Cum a -> Cum a -> Cum Bool
check e x y = do
  vx <- x
  vy <- y
  return (e vx vy)

kop :: Cum [a] -> Cum a
kop l = do
  vl <- l
  return (head vl)

staart :: Cum [a] -> Cum [a]
staart l = do
  vl <- l
  return (tail vl)

vind x l = do
  lg <- leeg l
  if lg
    then return False
    else do
      g <- check (==) x (kop l)
      if g
        then return True
        else vind x (staart l)

vindstijgend x l = do
  lg <- leeg l
  if lg
    then return False
    else
      let e = kop l
       in do
            g <- check (>) e x
            if g
              then return False
              else do
                eq <- check (==) x e
                if eq
                  then return True
                  else vindstijgend x (staart l)

funlijst :: ([a] -> [b]) -> Cum [a] -> Cum [b]
funlijst p l = do
  vl <- l
  return (p vl)

evallijst :: ([a] -> b) -> Cum [a] -> Cum b
evallijst e l = do
  vl <- l
  return (e vl)

pik :: Int -> Cum [a] -> Cum a
pik p l = do
  vl <- l
  return (vl !! p)

vindbs :: (Eq a, Ord a) => Cum a -> Cum [a] -> Cum Bool
vindbs x l = do
  len <- evallijst length l
  if len == 0
    then return False
    else
      let midden = len `div` 2
          mid = pik midden l
       in do
            fnd <- check (==) x mid
            if fnd
              then return True
              else do
                grtr <- check (>) x mid
                if grtr
                  then vindbs x (funlijst (drop (midden + 1)) l)
                  else vindbs x (funlijst (take midden) l)

test v w =
  let gewoon = vind (C 0 v) (C 0 [3, 6 .. 3 * w])
      stopoptijd = vindstijgend (C 0 v) (C 0 [3, 6 .. 3 * w])
      binair = vindbs (C 0 v) (C 0 [3, 6 .. 3 * w])
   in [(gewoon, geschiedenis gewoon), (stopoptijd, geschiedenis stopoptijd), (binair, geschiedenis binair)]
