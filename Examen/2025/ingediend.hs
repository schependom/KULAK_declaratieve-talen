import Control.Monad
import Data.Bits
import Data.Int

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
