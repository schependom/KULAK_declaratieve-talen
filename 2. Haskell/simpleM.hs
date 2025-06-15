import Prelude
import Control.Monad
{-
Eenvoudig voorbeeld van een Monad.
De gegevensstructuur 'Cum' houdt bij hoe vaak een bind werd uitgevoerd
om het resultaat te berekenen.
De eenvoudige functies plus1, telop en telop3 en priem gelden als voorbeeld.
De functie readCum leest een Cum van stdio. (indien a van klasse Read is (niet gezien))
De show van Cum toont enkel de waarde van het resultaat.
De functie 'geschiedenis' toont het aantal binds.

We sluiten af met een meer gevorderd voorbeeld
-}
-- =========================================================================
-- Voorwaarden om Cum een instantie van Monad te maken
instance Applicative Cum where
    pure = C 0    -- dit kan weggelaten worden, de Monad moet dan return definieren (deprecated)
    (<*>) = ap

instance Functor Cum where
    fmap = liftM
-- =========================================================================
data Cum a = C Int a

instance Show a => Show (Cum a) where
  show (C _ x) = show x 

-- de teller wordt beschouwd als een zij-effect
-- de onderstaande definitie zorgt ervoor dat Cum a aan de Monad wetten kan voldoen
instance Eq a => Eq (Cum a) where
  (==) (C _ x) (C _ y) = x == y

geschiedenis (C h _) = h

instance Monad Cum where
--  return = C 0 -- dit kan gebruikt worden ipv pure bij Applicative (deprecated)
  (>>=) (C b x) f = let C bf y = f x in C (bf+b+1) y 

lees::Read a => String -> Cum a
lees s = C 0 (read s)

leesIO::IO (Cum Int)
leesIO = do s <- getLine
            let v = lees s::Cum Int in
              return v

telopIO = do c1 <- leesIO
             c2 <- leesIO
             let r = telop c1 c2 in
              return r

vertelover::(Cum Int -> Cum Int -> Cum Int) -> IO String
vertelover f = do c1 <- leesIO
                  c2 <- leesIO
                  let r = f c1 c2 in
                    return ("resultaat : " ++ (show r) ++ ", geschiedenis : " ++ (show (geschiedenis r)))

plus1::Cum Int -> Cum Int
plus1 c = do v <- c
             return (v+1)   

telop::Cum Int -> Cum Int -> Cum Int
telop a b = do v1 <- a
               v2 <- b
               return (v1 + v2)

telop3 a b c = do v1 <- a
                  v2 <- b
                  v3 <- c
                  return (v1 + v2 + v3)

telop3r a b c = do v <- telop a b
                   v3 <- c
                   return (v + v3)
telopr a b = do v <- telop a b
                return v

prob y = if y > 0
            then do a <- (C 0 3)
                    b <- (C 0 4)
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

leeg::Eq a => Cum [a] -> Cum Bool
leeg l = do vl <- l
            return (vl == [])

check::(a->a->Bool) -> Cum a -> Cum a -> Cum Bool
check e x y = do vx <- x
                 vy <- y
                 return (e vx vy)

kop::Cum [a] -> Cum a
kop l = do vl <- l
           return (head vl)
staart::Cum [a] -> Cum [a]
staart l = do vl <- l
              return (tail vl)

vind x l = do lg <- leeg l
              if lg
               then return False
               else do g <- check (==) x (kop l)
                       if g
                       then return True
                       else vind x (staart l)
               
vindstijgend x l = do lg <- leeg l
                      if lg
                      then return False
                      else let e = kop l in
                             do g <- check (>) e x
                                if g 
                                then return False
                                else do eq <- check (==) x e
                                        if eq
                                        then return True
                                        else vindstijgend x (staart l)

funlijst::([a] -> [b]) -> Cum [a] -> Cum [b]
funlijst p l = do vl <-l
                  return (p vl)
evallijst::([a] -> b) -> Cum [a] -> Cum b
evallijst e l = do vl <- l
                   return (e vl)

pik::Int -> Cum [a] -> Cum a
pik p l = do vl <- l
             return (vl!!p)
                                     
vindbs::(Eq a, Ord a) => Cum a -> Cum [a] -> Cum Bool
vindbs x l = do len <- evallijst length l
                if len == 0
                then return False
                else let midden = len `div` 2
                         mid = pik midden l in
                         do fnd <- check (==) x mid
                            if fnd
                            then return True
                            else do grtr <- check (>) x mid
                                    if grtr
                                    then vindbs x (funlijst (drop (midden+1)) l)
                                    else vindbs x (funlijst (take midden) l) 
test v w = 
  let gewoon = vind (C 0 v) (C 0 [3,6..3*w])
      stopoptijd = vindstijgend (C 0 v) (C 0 [3,6..3*w])
      binair = vindbs (C 0 v) (C 0 [3,6..3*w])
  in [(gewoon,geschiedenis gewoon), (stopoptijd,geschiedenis stopoptijd),(binair,geschiedenis binair)]


-- Test op de Monad wetten
x5::Int -> Cum Int
x5 x = C 0 (x*5)

xvt b t x = C 0 (sum [x*l | l <- [b,t]])
someC::Cum Int = C 0 (-3)

testLaws = [x5 101 == (return 101 >>= x5),
            someC == (someC >>= return),
            (someC >>= x5 >>= (xvt (-10) 3)) == (someC >>= (\x -> (x5 x) >>= (xvt (-10) 3))),
            (someC >>= (xvt (-10) 3) >>= (x5)) == (someC >>= (\x -> ((xvt (-10) 3 x) >>= (x5))))]

