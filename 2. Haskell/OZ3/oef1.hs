{-
    1. Type Classes
-}

{-
    1.2 Expressions
-}

data MyBool = MyTrue | MyFalse

data Exp
  = Const MyBool
  | And Exp Exp
  | Or Exp Exp

-- 1.1
instance Eq MyBool where
  (==) :: MyBool -> MyBool -> Bool
  (==) MyTrue MyTrue = True
  (==) MyFalse MyFalse = True
  (==) _ _ = False

instance Eq Exp where
  (==) :: Exp -> Exp -> Bool
  Const bool1 == Const bool2 = bool1 == bool2
  And e1 e2 == And e3 e4 = e1 == e3 && e2 == e4
  Or e1 e2 == Or e3 e4 = e1 == e3 && e2 == e4

-- 1.2
instance Show MyBool where
  show :: MyBool -> String
  show MyTrue = "True"
  show MyFalse = "False"

instance Show Exp where
  show :: Exp -> String
  show (Const b) = show b
  show (And e1 e2) = show e1 ++ " && " ++ show e2
  show (Or e1 e2) = show e1 ++ " || " ++ show e2

-- 1.3

-- type class definiëren
class Evaluatable c where
  eval :: c -> Bool

-- 1.4

-- instanties aanmaken
instance Evaluatable MyBool where
  eval :: MyBool -> Bool
  eval MyTrue = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval :: Exp -> Bool
  eval (Const b) = eval b -- boolean
  eval (And e1 e2) = eval e1 && eval e2
  eval (Or e1 e2) = eval e1 || eval e2