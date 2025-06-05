import Text.Read

{-
Haskell. Gekwoteerde zitting.
Patrick De Causmaecker
21/5/2025, 9:30 - 12:30
-}

{-
  Catalan getallen zijn gedefinieerd als

1. C(n) = (2n)!/(n+1)!/n!
2. C(0) = 1 en C(n+1) = sum_{i=0}^{n} C(i)C(n-i)
3. C(0)=1 en C(n+1) = 2(2n+1)/(n+2) C(n)
4. Asymptotisch C(n) ~ 4^n/n^(3/2)/(pi)^(1/2)
5. Het aantal Dyck-woorden

Implementeer elk van deze formules en controleer.
-}
factorial::Integer->Integer
factorial n = product [1..n]

catalan::Integer -> Integer
catalan n = div (div (factorial (2*n)) (factorial (n+1))) (factorial n)

catalanSum::Integer -> Integer
catalanSum 0 = 1
catalanSum n = sum [(catalanSum i)*(catalanSum (n-1-i)) | i <- [0..(n-1)]]

catalanRec::Integer -> Integer
catalanRec 0 = 1
catalanRec n = div ((2*(2*n-1))*(catalanRec (n-1))) (n+1)

catalanAsy::Int -> Double
catalanAsy n = (4^n) / (((fromIntegral n)**(1.5))*(pi**(0.5)))

functielijst f = [f n|n <- [0..]]
lcatalan = functielijst catalan
lcatalanSum = functielijst catalanSum
lcatalanRec = functielijst catalanRec
lcatalanAsy = functielijst catalanAsy

testCatalan s e = 
  let ls l = (take (e+1-s) (drop s l))
    in 
      [(c == r,abs ((fromInteger c)-a)/(fromInteger c)) | (c,r,a) <- zip3 (ls lcatalan) (ls lcatalanRec) (ls lcatalanAsy)]

odds l = filter (\(n,x) -> odd x) [p|p <- zip [0..] l]
-- Algemener voorbeeld
cond c l = filter (\(n,x) -> c x) [p|p <- zip [0..] l]
{-
Alles van Van Dyck ($0.3$)

De oneindige rij allVanDyck bevat lijsten van strings van toenemende lengte: eerst deze van lengte 0, dan deze van lengte 2, enz. Dus bijvoorbeeld:

allVanDyck = [[" "],["()"],["(())","()()"],["((()))","(()())","(())()","()(())","()()()"],..]

1. Schrijf een functie $VanDyck\ n$ die de rij van Dyck-woorden van lengte $2n$ genereert.
2. Schrijf de Haskell functie $allVanDyck$ die de rij genereert.
3. Schrijf een functie $aantalVanDyck\ n$ die laat weten hoeveel Dyck-woorden van lengte $2n$ er bestaan.

-}
vanDyck n
 | n == 0 = [""]
 | otherwise = ["(" ++ d1 ++ ")" ++ d2 | k <- [1..n],d1 <- (vanDyck (n-k)),d2 <- (vanDyck (k-1))]

allVanDyck::[[String]]
allVanDyck = [vanDyck n | n <- [0..]]

aantalVanDyck n = length (vanDyck n)

{-
  Cantor set
1 2 3
1 2 3 . . . 7 8 9
1 2 3 . . . 7 8 9 . . . . . . . . . 19 20 21 . . . 25  26 27

                                       x.x
                                    xxx...xxx
                           xxx...xxx.........xxx...xxx
xxx...xxx.........xxx...xxx...........................xxx...xxx.........xxx...xxx

-}
cantor 0 = "x"
cantor p = let prev = (cantor (p-1)) in
             concat [prev,take (3^(p-1)) (repeat '.'),prev]

-- beveiligde versie, na een foute ingave kan men hoogstens nog h keer proberen
speelCantor h = do ax <- geefInt "Diepte: " h
                   if ax /= Nothing
                   then let Just x = ax
                      in do putStrLn (cantor x)
                            putStrLn "Nog eens? (j/n)"
                            a <- getLine
                            if a == "j" then speelCantor h
                                        else putStrLn "Bye"
                    else putStrLn "Gestopt."

-- veilige input van waarden
-- met als vraag s, na een foute ingave kan er hoogstens nog r keer opnieuw geprobeerd worden
geefInt::String -> Int -> IO (Maybe Int)
geefInt s r = do putStrLn s
                 as <- getLine
                 let a = readMaybe as in
                      if a == Nothing
                      then if r > 0 then geefInt (s++"??") (r-1)
                                    else do putStrLn "Teveel fouten!"
                                            return Nothing
                      else return a

-- En voor wie nog meer zou willen

{-
Een visualisatie van de cantor.
Na het kiezen van een positie wordt de kleinste diepte gekozen waarin 
die positie bestaat. Daarna krijgt de gebruiker de kans om in de cantor
heen en terug te wandelen.
-}

-- ========== hulpfuncties
-- Blijf de boodschap s herhalen tot een geheel getal is ingegeven
zekerInt::String -> IO Int
zekerInt s = do putStr s
                as <- getLine
                let a = readMaybe as in
                  if a == Nothing then zekerInt s
                                  else let Just v = a in return v
 
logBaseInt b x = go b x 1 0 where
                   go g o v acc = if (v*g) <= o then go g o (v*g) (acc + 1)
                                           else acc  
exp3Boven g = ceiling (logBase 3 (fromInteger (toInteger g)))

zoomCantor g = let d = exp3Boven g in cantor d

theCut g s = take 100 (drop g s)

-- ========== Gegeven een string mC en een positie g in die string, start een bezoek

showTheZoomie g mC = do putStrLn ("[" ++ (show g) ++ "," ++ show (g+100) ++ "] van " 
                                ++ (show (length mC)) ++ ": " ++ (theCut g mC))
                        putStrLn "l(inks)/r(echts)/p(ositie) ../h(ou op)"
                        a <- getChar
                        if a == 'p' then do ng <- zekerInt "Positie "
                                            showTheZoomie ng mC
                        else if a /= 'h' then
                          let ng = if a == 'l' then g-10
                                               else if a == 'r' then g+10
                                                                else g
                          in let fg = if ng < 0 
                                      then 0 
                                      else if ng >= (length mC) 
                                           then ((length mC)-10)
                                           else ng in do putStr "-> "
                                                         showTheZoomie fg mC
                        else putStrLn "\nBye"

-- =========== Bezoek aan een cantor
zoomieCan = do putStrLn "Op welke plaats wil je beginnen (ik kies dan iets dat net groot genoeg is) : "
               g <- zekerInt "? "
               putStrLn ("Ik plaats je op positie " ++ (show g) ++ ", van cantor " ++ (show (exp3Boven g)))
               let myCantor = (zoomCantor g) in
                     showTheZoomie g myCantor
               
