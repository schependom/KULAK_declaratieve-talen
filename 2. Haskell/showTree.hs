
{-
Noot bij de oefening over het tonen van een boom
Een andere versie van show, de complexiteit is verbonden met
de boomstructuur in de output.
-}

data Tree a = Node a (Tree a) (Tree a) | Nil

instance Show a => Show (Tree a) where
  show n = go "" "" n where 
    -- go s t n tekent boom n, voorafgegaan door s op de eerste lijn, en op elke volgende lijn door t  
    go _ _ Nil = ""
    go s t (Node x Nil (Node lx y z)) =
      s ++ (show x) ++ "\n" ++ t ++ "|\n" 
      ++ (go (t++"'--") (t++"   ") (Node lx y z))
    go s t (Node x (Node rx y z) Nil) =
      s ++ (show x) ++ "\n" 
      ++ (go (t ++ "'--") (t++"   ") (Node rx y z))
    go s t (Node x l r) =
      s ++ (show x) ++ "\n" 
      ++ (go (t ++ "|--") (t++"|  ") l) 
      ++ (go (t++"'--") (t++"   ")  r)
