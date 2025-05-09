{-
    5. Arithmetic Expressions (EXTRA)
-}

data Exp
  = Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Show, Eq)

-- 5.1: Interpreter
eval :: Exp -> Int
eval (Const i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

{-
ghci> eval (Add (Mul (Const 2) (Const 4)) (Const 3))
11

ghci> eval (Sub (Const 42) (Mul (Const 6) (Const 7)))
0
-}

-- 5.2: Compile it to a program of a simple STACK machine

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

-- program is modelled as a list of instructions
type Prog = [Inst] -- alias!

-- stack is modelled by a list of integers
type Stack = [Int]

{-
    Add (Mul (Const 2) (Const 4)) (Const 3)

        is equivalent aan stack programma (type Prog!)

    [IPush 2,IPush 4,IMul,IPush 3,IAdd]

        waarbij tussenresultaten op de stack komen (type Stack!)
-}

-- exceptions
-- voor bijvoorbeeld stack overflow
runtimeError :: Stack -- !!
runtimeError = error "Runtime error."

{-
    ghci> execute IAdd [0]
    \*** Exception: Runtime error.
-}

execute :: Inst -> Stack -> Stack
execute (IPush i) s = i : s
execute IAdd (x1 : x2 : xs) = (x1 + x2) : xs
execute ISub (x1 : x2 : xs) = (x2 - x1) : xs -- !!
execute IMul (x1 : x2 : xs) = (x1 * x2) : xs
execute IAdd [x1] = runtimeError
execute ISub [x1] = runtimeError
execute IMul [x1] = runtimeError

-- heel programma (lijst van Inst) runnen gegeven een initiele stack (lijst van Int)
run :: Prog -> Stack -> Stack
-- run [] s = s
run [] s = s
run (x : xs) s = run xs (execute x s)

{-
ghci> execute IAdd [4,5,6]
[9,6]

ghci> execute ISub [4,5,6]
[1,6]

ghci> execute (IPush 2) [4,5,6]
[2,4,5,6]

ghci> run [IAdd, ISub] [4,5,6]
[-3]
-}