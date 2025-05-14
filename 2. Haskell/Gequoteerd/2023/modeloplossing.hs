{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
import Text.Read

-- Dit is een optie voor veilige input

{-
1 De drempel (1 op 3)
-}

{-
1.1 erover (0.3)
1.1 erover (0.3)
De functie erover neemt een stijgende rij van gehele getallen en geeft de index terug van het eerste getal
dat groter is dan de eveneens gegeven grens.
-}
erover :: [Integer] -> Integer -> Int
erover (x : l) grens
  | x > grens = 0
  | otherwise = 1 + erover (l) grens

{-
1.2 erover_f (0.2)
De functie erover_f neemt een functie die een geheel getal afbeeldt op een element van een type dat
vergelijking toelaat en geeft de index terug van het eerste getal dat groter is dan de eveneens gegeven grens.
-}
erover_f :: (Ord a) => (Int -> a) -> Int -> a -> Int
erover_f f van grens = go van
  where
    go s
      | (f s) > grens = s
      | otherwise = go (s + 1)

{-
1.3 fiberover (0.3)
De functie fiberover doet hetzelfde als de functie erover f, maar ze gebruikt fibonacci rijen om de grens
sneller te overschrijden. (Laat het argument toenemen als een rij van fibonacci tot de grens wordt
overschreden. Start dan opnieuw van de voorlaatste index en laat opnieuw het argument toenemen als
een rij van fibonacci. Stop als het verschil in index van de waarde juist onder de grens en die
juist boven de grens gelijk is aan 1.)
-}
fiberover :: (Ord a) => (Int -> a) -> Int -> a -> Int
fiberover f van grens =
  let nb = go f van 1 1 grens
   in if f (nb + 1) > grens
        then (nb + 1)
        else fiberover f nb grens
  where
    go f van fibo1 fibo2 grens
      | f (van + fibo1) > grens = van
      | otherwise = go f (van + fibo1) fibo2 (fibo1 + fibo2) grens

{-
1.4 En rapporteer (0.2)
De functie fiberoveri_l doet hetzelfde als de functie fiberover, maar ze geeft ook een lijst van
gevalueerde argumenten terug.
-}
fiberover_l :: (Ord a) => (Int -> a) -> Int -> a -> (Int, [Int])
fiberover_l f van grens =
  let nbso = go f van 1 1 grens
      nb = fst nbso
      lb = snd nbso
   in if f (nb + 1) > grens
        then (nb + 1, lb ++ [nb + 1])
        else
          let tr = fiberover_l f nb grens
           in (fst tr, lb ++ ((nb + 1) : (snd tr)))
  where
    go f van fibo1 fibo2 grens
      | f (van + fibo1) > grens = (van, [van + fibo1])
      | otherwise =
          let gts = go f (van + fibo1) fibo2 (fibo1 + fibo2) grens
           in (fst gts, (van + fibo1) : (snd gts))

{-
2 Iets met priemfactoren (1 op 3)
-}

{-
2.1 Geen delers (0.3)
Schrijf een functie nondiv die True terug geeft asa het eerste argument geen deler heeft in de lijst die als tweede
argument wordt meegegeven.
-}
nondiv :: Int -> [Int] -> Bool
nondiv x [] = True
nondiv x (y : l) = ((x `mod` y) /= 0) && (nondiv x l)

{-
2.2 Naief, maar goed (0.1)
Schrijf een recht-toe recht-aan functie die True teruggeeft a.s.a. het argument een priemgetal is.
-}
priem_r_t_r_a p = (p > 1) && nondiv p [2 .. (p - 1)]

{-
2.3 Klassiek (0.3)
Schrijf een functie priem f die een lijst van een aantal priemgetallen teruggeeft. De functie gebruikt de zeef van
Eratosthenes die een getal aanvaardt als geen van de priemgetallen waarvan het kwadraat niet groter is dan
dat getal dit getal delen. De vorm waarin je deze functie schrijft is vrij.
-}
priem_f :: Int -> [Int]
priem_f n
  | n == 1 = [2]
  | otherwise =
      let p = priem_f (n - 1)
          lp = last p
       in p ++ [head [x | x <- [(lp + 1) ..], nondiv x (takeWhile (\y -> y * y <= x) p)]]

{-
2.4 Als een recursief gedefinieerde lijst (0.3)
Definieer priem als een oneindige lijst van alle priemgetallen in stijgende volgorde. Gebruik een recursieve
constructie die de zeef van Eratosthenes implementeert met behulp van takeWhile.
-}
priem = 2 : [x | x <- [3 ..], (nondiv x (takeWhile (\y -> y * y <= x) priem))]

{-
3 Een beetje IO (1 op 3)
Schrijf een programma dat toelaat een ’tekening’ te maken. Het commando teken tekent een bord met een hoogte en een
breedte en laat vervolgens toe om puntjes te zetten. (0.7) Je kan het commando fout-tolerant maken op twee manieren:
• controleer op de ingegeven cijfers en vraag opnieuw indien de gebruiker een teken ingeeft dat niet als
een getal kan worden gelezen (0.2).
• controleer op de input en vraag opnieuw indien de grenzen niet werden gerespecteerd of
iets anders dan ’ja’ of ’nee’ werd ingegeven (0.1).
-}

-- de dialoog
teken :: Int -> Int -> IO ()
teken h b =
  let r = rooster h b
   in go r
  where
    go :: [[Char]] -> IO ()
    go rooster = do
      legbord rooster b
      putStrLn "Tekenen (j/n)?"
      a <- getLine
      if a == "j"
        then do
          ph <- geef "Hoogte?" 0 h
          pb <- geef "Breedte?" 0 b
          let nr = zetpunt rooster ph pb
           in go nr
        else
          if a == "n"
            then do putStrLn "Bye"
            else do
              putStrLn "Sorry, ik begreep dat niet"
              go rooster

-- veilige input van waarden in het interval [l,m[
-- met als vraag s
geef :: String -> Int -> Int -> IO Int
geef s l m = do
  putStrLn s
  as <- getLine
  let a = readMaybe as
   in if a == Nothing
        then geef (s ++ "??") l m
        else
          let Just r = a
           in if r < l || r >= m
                then geef (s ++ "??") l m
                else return r

-- definitie van het rooster
rooster h b = replicate h (replicate b ' ')

-- teken het bord
legbord :: [[Char]] -> Int -> IO ()
legbord r b =
  do
    print (replicate b '=')
    go r
    print (replicate b '=')
  where
    go r
      | r == [] = do return ()
      | otherwise = do
          print (head r)
          go (tail r)

-- zet een punt
zetpunt r ph pb = ((take ph r) ++ [zetpol (r !! ph) pb] ++ (drop (ph + 1) r))

zetpol l p = (take p l) ++ ['.'] ++ (drop (p + 1) l)
