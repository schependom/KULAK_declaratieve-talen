{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

import Control.Monad

{-# HLINT ignore "Use newtype instead of data" #-}

{--
\* Our State Monad
--}

type SP s a = s -> (a, s)

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
  pure :: a -> State s a
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
  (>>=) :: State s a -> (a -> State s b) -> State s b
  -- msp = monad van state passing = State (sp s a)
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