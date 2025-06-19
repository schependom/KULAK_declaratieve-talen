type StateMap = [(Int, Int)]

-- we kunnen volgende functie gebruiken om de value (tweede item in tupel)
-- te vinden bij een key (eerste item in tupel)

-- * lookup :: Eq a => a -> [(a,b)] -> Maybe b

-- =====
-- = 1 =
-- =====

nextCollatz :: Int -> Int
nextCollatz y
  | even y = div y 2
  | otherwise = 3 * y + 1

mcoll :: Int -> StateMap -> (Int, StateMap)
mcoll start huidigeState
  | start == 1 = (0, [(0, 1)])
  | otherwise =
      let mRes = lookup start huidigeState
       in case mRes of
            -- Gevonden in geheugen
            Just aantalStappen -> (aantalStappen, huidigeState)
            -- Niet gevonden in geheugen
            Nothing ->
              let volgendGetal = nextCollatz start
                  (volgendAantalStappen, volgendeState) = mcoll volgendGetal huidigeState
                  nieuweState = (start, volgendAantalStappen + 1) : volgendeState
               in (volgendAantalStappen + 1, nieuweState)

-- =====
-- = 2 =
-- =====

-- memoColl returnt een State datatype
-- met daarin een state-passing functie
-- s -> (a,s) met s=StateMap en a=Int
--
-- memoColl is met andere woorden een functie
-- f :: a -> (SP s b)
-- We kunnen hier later dus een bind op uitvoeren:
-- (>>=) :: State (SP s a) -> (a -> SP s b) -> (SP s b)

-- partiele functie-applicatie op mcoll (mcoll x)
-- leidt tot signatuur
--    StateMap -> (Int, StateMap)
-- wat precies is wat we nodig hebben, maar dan nog zonder State-schil

memoColl :: Int -> State StateMap Int
memoColl x = State (mcoll x)

-- =====
-- = 3 =
-- =====

-- het resultaat is van het type
-- State s a
-- State StateMap a
-- State (SP s a)
-- State (s -> (a,s))

-- Uit de voorbeelden leiden we af dat
-- a = [(Int, Int)]

-- De <- is een bind:
--
-- a' <- memoColl a
--    is equivalent aan
-- do memoColl a >>= \a' -> f a'

-- memoColl a produceert een State (s -> (a,s))
-- de bind haalt de State er van rond, berekent een nieuwe
-- s1 op basis van s0 (input met lambda) door hem aan s->(a,s) te geven
-- en past de functie f toe op a, hetgeen een SP s b
-- oplevert -> de s1 wordt hieraan gegeven

largestAbove :: Int -> Int -> State StateMap [(Int, Int)]
largestAbove minGrootte startGetal = do
  aantalStappen <- memoColl startGetal
  let groterDanMin = aantalStappen >= minGrootte
      newMin =
        if groterDanMin
          then aantalStappen + 1
          else minGrootte
  rest <- largestAbove newMin (startGetal + 1)
  return $
    if groterDanMin
      then (startGetal, aantalStappen) : rest
      else rest

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
  fmap fab ma = ma >>= \x -> return $ fab x

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
