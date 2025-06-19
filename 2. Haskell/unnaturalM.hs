import Control.Monad
import Data.List
import Prelude

{-
Bij het bewijs van de associativiteit van (>>=) wordt
gebruik gemaakt van het 'naturaliteits' beginsel.
Dit begrip gaat verder dan bedoeld in deze
inleidende cursus.

In de Monad context evenwel betekent het

join (fmap (fmap f) m) == (fmap f) (join m)
  of
join . (fmap (fmap f)) == (fmap f) . join

en dit kan men wel inzien: als de join geen informatie
gebruikt over het type a in m a, dan doet het er niet toe
of men eerst een laag van m (m a) weghaalt en dan de
functie f toepast op overblijvende laag (rechterlid),
of eerst f toepast op de onderste laag en dan pas weghaalt (linkerlid).
Zoals Monads in Haskell vandaag geimplementeerd zijn kan
men geen constraints opleggen aan het type a en join
heeft dus geen informatie. De volgende voorbeelden illustereren dit.

Hieronder twee voorbeelden, een op lijsten en een op bomen.

De m(b)Join die hieronder gebruikt worden lijken misschien
onnatuurlijk, voldoen niet aan de Monad wetten,
maar zijn wel natuurlijk.

Telkens wordt een onnatuurlijke versie gegeven die wel informatie
over het type a gebruikt.

(doeDeTesten, doTests, doTestsB)
-}

data UnnaM a = U [a]
  deriving (Eq, Ord, Show)

-- =========================================================================
-- Voorwaarden om UnnaM een instantie van Monad te maken
instance Applicative UnnaM where
  pure :: a -> UnnaM a
  pure x = U [x] -- dit kan weggelaten worden, de Monad moet dan return definieren (deprecated)
  (<*>) :: UnnaM (a -> b) -> UnnaM a -> UnnaM b
  -- [(<*>) = ap] kan ook!
  mfab <*> ma = do
    fab <- mfab
    a <- ma
    return $ fab a

instance Functor UnnaM where
  fmap :: (a -> b) -> UnnaM a -> UnnaM b
  fmap f ma = do
    a <- ma
    return $ f a

-- ! fmap_M :: (a -> b) -> m a -> m b
-- ! fmap_M f ma = ma >>= \x -> return $ f x

-- =========================================================================

instance Monad UnnaM where
  (>>=) :: UnnaM a -> (a -> UnnaM b) -> UnnaM b
  (>>=) u f = mJoin (mFmap f u)

mFmap :: (a -> b) -> UnnaM a -> UnnaM b
mFmap _ (U []) = U []
mFmap f (U (x : xs)) =
  let fl = f x
      U r = mFmap f (U xs)
   in U (fl : r)

-- Een heel vreemde versie van join...
-- De eerste 10 elementen van de lijsten in variabelen van het type UnnaM (UnnaM a) worden gesorteerd op de lengte
-- van de lijsten in hun elementen, voor zover deze lijsten niet meer dan 100 elementen bevatten...
-- Blijkt niet aan de wet join.join == join.(fmap join) te voldoen
mJoin :: UnnaM (UnnaM a) -> UnnaM a
-- mJoin (U []) = U []
-- mJoin (U ((U l):ru)) = let U r = mJoin (U ru) in U (l++r)
mJoin u = go (sortU u)
  where
    go (U []) = U []
    go (U ((U l) : ru)) = let U r = go (U ru) in U (l ++ r)

sortU :: UnnaM (UnnaM a) -> UnnaM (UnnaM a)
sortU (U l) = U (sortBy compLengths (take 100 l) ++ drop 100 l)

length100 :: [a] -> Int
length100 l = length (take 100 l)

compLengths :: UnnaM a1 -> UnnaM a2 -> Ordering
compLengths (U l1) (U l2) = compare (length100 l1) (length100 l2)

-- Als we iets weten over type a in UnnaM (UnnaM a) kunnen we naturaliteit wel breken
-- Onderstel bijvoorbeeld dat we Ord a kunnen veronderstellen
-- De eerste 10 elementen van de lijsten in variabelen van het type UnnaM (UnnaM a) worden nu gesorteerd
-- op basis van de ordening van a
nnJoin :: (Ord a) => (UnnaM (UnnaM a)) -> (UnnaM a)
nnJoin u = go (sortnnU u)
  where
    go (U []) = U []
    go (U ((U l) : ru)) = let U r = go (U ru) in U (l ++ r)

sortnnU :: (Ord a) => (UnnaM (UnnaM a)) -> (UnnaM (UnnaM a))
sortnnU (U l) = U ((sort (take 10 l)) ++ (drop 10 l))

-- =========================== TESTS
-- Twee functies
g :: Integer -> UnnaM Integer
g x = U [-10 .. x]

f :: Integer -> Integer -> UnnaM Integer
f z x = U [-x * y | y <- [1 .. z]]

-- Een UnnaM Integer
uI = U [U [1, 2, 3], U [3, 4, 5], U [10]]

-- In de versie zonder Ord a
-- Een test op de naturaliteit
testNaturalU :: (Eq a) => (a -> UnnaM a) -> UnnaM (UnnaM a) -> Bool
testNaturalU fie u = (mJoin . (mFmap (mFmap fie))) u == ((mFmap fie) . mJoin) u

testNatl = [testNaturalU g uI, testNaturalU (f 15) uI]

-- Test de wetten
testLaws =
  [ (theDouble 7581) == (return 7581 >>= theDouble),
    uI == (uI >>= return),
    ((U [1, 2, 3]) >>= theDouble >>= theTriple) == ((U [1, 2, 3]) >>= (\x -> ((theDouble x) >>= theTriple))),
    ((U [1, 2, 3]) >>= (f 15) >>= g) == ((U [1, 2, 3]) >>= (\x -> ((f 15 x) >>= g)))
  ]

testLaw1 t x = (show (t x)) ++ "==" ++ (show (return x >>= t))

testLaw2 u = (show u) ++ "==" ++ (show (u >>= return))

testLaw3 t1 t2 u = do
  putStrLn ((show (u >>= t1 >>= t2)) ++ "==" ++ (show (u >>= (\x -> ((t1 x) >>= t2)))))
  print ((u >>= t1 >>= t2) == (u >>= (\x -> ((t1 x) >>= t2))))

-- In de versie met Ord a
-- Een test op de naturaliteit
testnnNatural :: (Eq a, Ord a) => (a -> UnnaM a) -> UnnaM (UnnaM a) -> Bool
testnnNatural fie u = (nnJoin . (mFmap (mFmap fie))) u == ((mFmap fie) . nnJoin) u

-- g bewaart de ordening ([x,y,...] < [v,w,...] => [g x,g y,...] < [g v,g w,...]
-- f bewaart de ordening niet door het minteken in de definitie
-- we verwachten testnnNatl = [True,False]
testnnNatl = [testnnNatural g uI, testnnNatural (f 15) uI]

-- Test de wetten
-- We verwachten dat de wetten voldaan blijven
(!>>=) u f = nnJoin (mFmap f u) -- dit kan niet gebruikt worden voor de Monad operator >>= (Ord a voorwaarde)

testnnLaws =
  [ (theDouble 7581) == (return 7581 !>>= theDouble),
    uI == (uI !>>= return),
    ((U [1, 2, 3]) !>>= theDouble !>>= theTriple) == ((U [1, 2, 3]) !>>= (\x -> ((theDouble x) !>>= theTriple))),
    ((U [1, 2, 3]) !>>= (f 15) !>>= g) == ((U [1, 2, 3]) !>>= (\x -> ((f 15 x) !>>= g)))
  ]

testnnLaw1 t x = (show (t x)) ++ "==" ++ (show (return x !>>= t))

testnnLaw2 u = (show u) ++ "==" ++ (show (u !>>= return))

testnnLaw3 t1 t2 u =
  do
    putStrLn ((show (u !>>= t1 !>>= t2)) ++ "==" ++ (show (u !>>= (\x -> ((t1 x) !>>= t2)))))
    print ((u !>>= t1 !>>= t2) == (u !>>= (\x -> ((t1 x) !>>= t2))))

-- Hulpfuncties
theDouble x = U [2 * x]

theTriple x = U [3 * x]

doTests = do
  putStrLn "=============== testen op de Monad UnnaM ================"
  putStrLn "Test op de MonadPlus wetten ([eerste,tweede,derde,derde])"
  print testLaws
  putStrLn "Test op naturaliteit"
  print testNatl
  putStrLn "=============== met kennis over het type a in UnnaM a ===="
  putStrLn "Test op de MonadPlus wetten ([eerste,tweede,derde,derde])"
  print testnnLaws
  putStrLn "Test op naturaliteit"
  print testnnNatl

-- =========================================================================
-- Tweede voorbeeld : BnnaM
-- Een heel kleine wijziging: een teller voor het aantal uitgevoerde join's
-- zorgt ervoor dat de wetten niet meer voldaan zijn
-- Aangezien we het type a niet kunnen beperken, blijft de naturaliteit gelden
-- =========================================================================
data BnnaM a = B Int (BnnaM a) (BnnaM a) | BL a deriving (Eq, Show, Ord)

-- Voorwaarden om BnnaM een instantie van Monad te maken
instance Applicative BnnaM where
  pure x = BL x
  (<*>) = ap

instance Functor BnnaM where
  fmap = liftM

-- =========================================================================

instance Monad BnnaM where
  --  (>>=)::BnnaM a -> (a -> BnnaM b) -> BnnaM b
  (>>=) u f = bJoin (bFmap f u)

bFmap :: (a -> b) -> (BnnaM a) -> (BnnaM b)
bFmap f (B c l r) = B c (bFmap f l) (bFmap f r)
bFmap f (BL l) = BL (f l)

bJoin :: (BnnaM (BnnaM a)) -> (BnnaM a)
bJoin (B c l r) = B (c + 1) (bJoin l) (bJoin r)
bJoin (BL l) = l

-- we voeren nu opnieuw de constraint Ord a in
-- en gebruiken die om twee bomen te vergelijken

bbJoin :: (Ord a) => (BnnaM (BnnaM a)) -> (BnnaM a)
bbJoin (B c l r) =
  if (l < r)
    then B (c + 1) (bbJoin l) (bbJoin r)
    else B (c + 1) (bbJoin r) (bbJoin l)
bbJoin (BL l) = l

(!!>>=) :: (Ord a, Ord b) => BnnaM a -> (a -> BnnaM b) -> BnnaM b
(!!>>=) b f = bbJoin (bFmap f b)

-- =========================== TESTS BnnaM
-- Twee functies
gb :: Integer -> Integer -> BnnaM Integer
gb d x
  | d <= 0 = BL x
  | odd d = B 0 (gb (d - 1) x) (gb (d - 2) (x + 1))
  | otherwise = B 0 (gb (d - 2) x) (gb (d - 1) (x + 1))

fbi :: Integer -> Integer -> Integer -> BnnaM Integer
fbi d z x
  | z <= 0 = BL x
  | d <= z = BL x
  | otherwise = B 0 (fbi (d - z) z x) (fbi z (z `div` 2) (x + z))

fbs d z x = fbi d z (-x)

someB = B 0 (B 0 (BL 1) (BL 3)) (B 0 (B 0 (BL 4) (BL 3)) (BL 2))

someBB = B 0 (B 0 (BL (BL 1)) (BL (BL 3))) (B 0 (B 0 (BL (BL 4)) (BL (BL 3))) (BL (BL 2)))

-- Een test op de naturaliteit van BnnaM
testNaturalB :: (Eq a) => (a -> BnnaM a) -> BnnaM (BnnaM a) -> Bool
testNaturalB fie u = (bJoin . (bFmap (bFmap fie))) u == ((bFmap fie) . bJoin) u

testNatlB =
  [ testNaturalB (gb 5) someBB,
    testNaturalB (fbi 15 5) someBB,
    testNaturalB (fbs 15 5) someBB
  ]

-- Test de wetten
testLawsB =
  [ (gb 5 81) == (return 81 >>= (gb 5)),
    someB == (someB >>= return),
    (someB >>= (gb 5) >>= (fbi 3 2)) == (someB >>= (\x -> ((gb 5 x) >>= (fbi 3 2)))),
    (someB >>= (fbi 3 2) >>= (gb 5)) == (someB >>= (\x -> ((fbi 3 2 x) >>= (gb 5))))
  ]

testBLaw1 t x = (show (t x)) ++ "==" ++ (show (return x >>= t))

testBLaw2 u = (show u) ++ "==" ++ (show (u >>= return))

testBLaw3 t1 t2 u = do
  putStrLn ((show (u >>= t1 >>= t2)) ++ "==" ++ (show (u >>= (\x -> ((t1 x) >>= t2)))))
  print ((u >>= t1 >>= t2) == (u >>= (\x -> ((t1 x) >>= t2))))

-- In de versie met Ord a
testbbLawsB =
  [ (gb 5 81) == (return 81 !!>>= (gb 5)),
    someB == (someB !!>>= return),
    (someB !!>>= (gb 5) !!>>= (fbi 3 2)) == (someB !!>>= (\x -> ((gb 5 x) !!>>= (fbi 3 2)))),
    (someB !!>>= (fbi 3 2) !!>>= (gb 5)) == (someB !!>>= (\x -> ((fbi 3 2 x) !!>>= (gb 5))))
  ]

-- Een test op de naturaliteit
testbbNatural :: (Eq a, Ord a) => (a -> BnnaM a) -> BnnaM (BnnaM a) -> Bool
testbbNatural fie u = (bbJoin . (bFmap (bFmap fie))) u == ((bFmap fie) . bbJoin) u

testbbNatlB =
  [ testbbNatural (gb 5) someBB,
    testbbNatural (fbi 15 5) someBB,
    testbbNatural (fbs 15 5) someBB
  ]

doTestsB = do
  putStrLn "=============== testen op de Monad BnnaM ================"
  putStrLn "Test op de MonadPlus wetten ([eerste,tweede,derde,derde])"
  print testLawsB
  putStrLn "Test op naturaliteit"
  print testNatlB
  putStrLn "=============== met kennis over het type a in UnnaM a ===="
  putStrLn "Test op de MonadPlus wetten ([eerste,tweede,derde,derde])"
  print testbbLawsB
  putStrLn "Test op naturaliteit"
  print testbbNatlB

doeDeTesten = do
  doTests
  doTestsB
