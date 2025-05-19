module MOPL where

import Data.List
import Data.Maybe (fromJust)

-- Exercise 1
data Term = IntTerm Int | VarTerm String | Add Term Term | Subtract Term Term | Multiply Term Term

data Statement = Assign String Term | Print Term

-- Exercise 2
assign :: String -> Term -> Statement
assign = Assign

printTerm :: Term -> Statement
printTerm = Print

intTerm :: Int -> Term
intTerm = IntTerm

varTerm :: String -> Term
varTerm = VarTerm

plus :: Term -> Term -> Term
plus = Add

times :: Term -> Term -> Term
times = Multiply

minus :: Term -> Term -> Term
minus = Subtract

-- Exercise 3
type State = [(String, Int)]

valueOf' :: State -> String -> Int
valueOf' sList s = snd $ sList !! fromJust (findIndex (\(string, _) -> string == s) sList)

valueOf :: State -> String -> Int
valueOf sList s = fromJust (lookup s sList)

valueOf'' :: State -> String -> Int
valueOf'' [] _ = 0
valueOf'' (x : xs) s
  | fst x == s = snd x
  | otherwise = valueOf'' xs s

insertS :: String -> Int -> State -> State
insertS s i [] = [(s, i)]
insertS s i (x : xs)
  | fst x == s = (s, i) : xs
  | otherwise = x : insertS s i xs

-- Exercise 4
evalTerm :: State -> Term -> Int
evalTerm s term = case term of
  IntTerm i -> i
  VarTerm v -> valueOf s v
  Add t1 t2 -> evalBinOp (+) t1 t2
  Subtract t1 t2 -> evalBinOp (-) t1 t2
  Multiply t1 t2 -> evalBinOp (*) t1 t2
  where
    evalBinOp :: (Int -> Int -> Int) -> Term -> Term -> Int
    evalBinOp op t1 t2 = op (evalTerm s t1) (evalTerm s t2)

-- Exercise 5
execAssign :: String -> Term -> State -> State
execAssign s t state = insertS s (evalTerm state t) state

-- Exercise 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure s [] = s
execPure s (p : ps) = case p of
  Assign str t -> execPure (execAssign str t s) ps
  Print _ -> execPure s ps

-- Exercise 7

program :: [Statement]
program =
  [ assign "a" (intTerm 8),
    printTerm (plus (varTerm "a") (intTerm (-5))),
    assign "b" (plus (varTerm "a") (intTerm 2)),
    assign "a" (plus (varTerm "a") (varTerm "b")),
    printTerm (varTerm "a")
  ]

execute :: Program -> IO ()
execute p = do
  executePrints [] p
  where
    executePrints :: State -> Program -> IO ()
    executePrints _ [] = return ()
    executePrints s (stmt : stmts) = case stmt of
      Print t -> do
        print (evalTerm s t)
        executePrints (execPure s [stmt]) stmts
      Assign str term ->
        executePrints (execAssign str term s) stmts
