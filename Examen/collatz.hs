{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import qualified Data.Map as M

---------
--- 1 ---
---------

nextCollatz :: Int -> Int
nextCollatz x = if even x then x `div` 2 else 3 * x + 1

type Smap = M.Map Int Int

-- x = startpunt collatzrij
-- s = toestand geheugen
-- sn = nieuwe toestand geheugen
mcoll :: Int -> Smap -> (Int, Smap)
mcoll x s
  | x == 1 = (0, M.insert 1 0 s) -- Basisgeval: al bereikt in 'mcoll' initiële stap, maar voor de zekerheid.
  | otherwise =
      case M.lookup x s of
        -- Als het resultaat voor 'x' al in het geheugen zit
        -- return het dan direct.
        Just len -> (len, s)
        -- Zo niet, bereken de volgende stap in de Collatz-reeks.
        Nothing ->
          let -- Bereken de volgende term in de Collatz-reeks.
              next_n = nextCollatz x
              -- Roep recursief calculateCollatz aan voor de volgende term
              -- met het huidige geheugen.
              (v_next, sn_next) = mcoll next_n s
              -- De lengte voor de huidige 'x' is 1 (voor deze stap) plus de lengte van de rest van de reeks.
              v = 1 + v_next
              -- Voeg het berekende resultaat voor 'x' toe aan het geheugen.
              sn = M.insert x v sn_next
           in (v, sn)

-- ! Merk op !
-- Partiele applicatie (enkel Int) geven leidt tot een signatuur
-- Smap -> (Int, Smap) = s -> (a,s) = SP s a = SP Smap Int

-- s = (M.Map Int Int)
-- (runState (memoColl x))  :: SP s a = s -> (a,s)
--                          :: (M.Map Int Int) -> (Int, M.Map Int Int)
--                          :: Smap -> (Int, Smap)

---------
--- 2 ---
---------

-- Aantal stappen tot de waarde 1 bereikt wordt vanuit x
-- gebruikmakend van de collatz-regels en van het geheugen
-- in Smap == M.Map Int Int
memoColl :: Int -> State Smap Int
memoColl x = State (mcoll x)

{-
ghci> :t mcoll 2
mcoll 69  :: Smap -> (Int, Smap)
          :: s -> (a,s)
          :: SP s a
-}

---------
--- 3 ---
---------

-- De State Monad is hier
--      State (M.Map Int Int) (...?)
--
-- dus
--      s = Smap = (M.Map Int Int)
--      a = [(Int, Int)]

-- sp gaat van s naar (a, s)

{-
! do <- State (SP s a) krijgt de WAARDE a!

Er geldt namelijk dat
  do
    x <-  State (SP s a)
    ...
gelijk is aan
  do
    State (SP s a) >>= \x -> ...

! Dus de x is de waarde van type a van de (SP s a) = s -> (a,s)
! die wordt doorgegeven aan de functie waarnaar wordt gebind.

Niet de state
-}

largestAbove :: Int -> Int -> State Smap [(Int, Int)]
largestAbove minAantalStappen n = do
  aantalStappen <- memoColl n
  let groterDanMinimum = aantalStappen >= minAantalStappen
      newmin =
        if groterDanMinimum
          then aantalStappen + 1
          else minAantalStappen
  rest <- largestAbove newmin (n + 1)
  return
    ( if groterDanMinimum
        then (n, aantalStappen) : rest
        else rest
    )

{--
\* Our State Monad
--}

-- alias
-- ! let op: geen \s -> ...!!
type SP s a = s -> (a, s)

-- kind?  (State s) :: * -> *
--        a :: *
-- Samen
--        (State s a) :: *
data State s a = State (SP s a)

{--
\* Functor and Applicative defaults for Monad instance
--}
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap fab ma = do
    a <- ma
    return (fab a)

-- of...
{-
fmap fab ma = liftM fab ma
fmap fab ma = ma >>= \x -> return $ fab x
fmap fab ma = ma >>- return . fab
-}

{-
  ! Om iets een Monad instantie te maken, moet het ook een instantie hebben van:
      1. Functor (logisch!), want [fmap :: a -> b -> f a -> f b] nodig.
      2. Applicative, want nodig:
          - pure  :: a -> appl a
          - <*>   :: appl (a -> b) -> appl a -> appl b
-}
-- Anders 'no instance for applicative'
instance Applicative (State s) where
  -- 'return' is deprecated voor Monad -> definieer in Applicative, want dat is een superklasse.
  -- pure :: a -> State s a
  pure a = do return a

  -- functie in State context toepassen op State
  (<*>) :: State s (a -> b) -> State s a -> State s b
  mfab <*> ma = do
    fab <- mfab
    a <- ma
    return (fab a)

{--
\* The State Monad as defined in 'Introduction to Haskell' by Tom Schrijvers
--}
{-
Applicative is een SUPERKLASSE van monad!

class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
-}
instance Monad (State s) where
  -- eigenlijk niet nodig, want we hebben al pure in superklasse Applicative
  return :: a -> State s a
  return x = State (\s -> (x, s))

  -- msp = monad van state passing = State (sp s a)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  msp >>= f =
    State
      ( \s0 ->
          let (x, s1) = (runState msp) s0
           in (runState (f x)) s1
      )

{--
\* runState removes the 'State' constructor
\* result is a function s -> (a,s) from a State Monad
--}
runState :: State s a -> SP s a
runState (State m) = m

{--
\* modify adds the 'State' constructor
\* to a function s -> (a,s) to obtain a State Monad
--}
modify :: SP s a -> State s a
modify m = State m
