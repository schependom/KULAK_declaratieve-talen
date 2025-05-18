module RhymesTest where

-- import RhymesSolution

import Control.Exception (SomeException (..), catch)
import Control.Monad (liftM2)
import Data.Monoid
import RhymesSkeleton

program :: String
program = "PROGRAM P-R-OW1-G-R-AE2-M N programs"

paleontologist :: String
paleontologist = "PALEONTOLOGIST P-EY2-L-IY0-AH0-N-T-AA1-L-AH0-JH-IH0-S-T N paleontologists"

whale :: String
whale = "WHALE W-EY1-L V whaled whaling whales"

db :: [Entry]
db =
  map
    parseEntry
    [ "FRAIL F-R-EY1-L A frailer frailest",
      "ALE EY1-L N ales",
      "ASSAIL AH0-S-EY1-L V assailed assailing assails",
      "PALE P-EY1-L A paler palest"
    ]

main :: IO ()
main = do
  putStrLn "Suppose the following assignments:"
  putStrLn "let program = \"PROGRAM P-R-OW1-G-R-AE2-M N programs\""
  putStrLn "let paleontologist = \"PALEONTOLOGIST P-EY2-L-IY0-AH0-N-T-AA1-L-AH0-JH-IH0-S-T N paleontologists\""
  putStrLn "let whale = \"WHALE W-EY1-L V whaled whaling whales\""
  putStrLn "let db = map parseEntry\n\t[\"FRAIL F-R-EY1-L A frailer frailest\",\n\t\"ALE EY1-L N ales\",\n\t\"ASSAIL AH0-S-EY1-L V assailed assailing assails\",\n\t\"PALE P-EY1-L A paler palest\"]"
  startTests
    <> test "(show . parseEntry) program == program" ((show . parseEntry) program == program)
    <> test "(show . parseEntry) paleontologist == paleontologist" ((show . parseEntry) paleontologist == paleontologist)
    <> test "countSyllables (parseEntry program) == 2" (countSyllables (parseEntry program) == 2)
    <> test "countSyllables (parseEntry paleontologist) == 6" (countSyllables (parseEntry paleontologist) == 6)
    <> test "any (rhymes (parseEntry program)) db  == False" (not $ any (rhymes (parseEntry program)) db)
    <> test "all (rhymes (parseEntry whale)) db == True" (all (rhymes (parseEntry whale)) db)
    <> test "noPrefix [[1,2],[0,1,2]] == True" (noPrefix [[1 :: Integer, 2], [0, 1, 2]])
    <> test "noPrefix [[1,2],[1,2,3]] == False" (not $ noPrefix [[1 :: Integer, 2], [1, 2, 3]])
    <> test "makeSentence [parseEntry program] == []" (null (makeSentence [parseEntry program]))
    <> test "makeSentence db == [\"the frail ale assails pale\"]" (makeSentence db == ["the frail ale assails pale"])
    >>= endTests

-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b =
  do
    notImplemented <- isUndefined b
    case notImplemented of
      True -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
      False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
      _ -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where
    printResult colorCode result =
      putStrLn $ "Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed) =
  case (getSum notImpl, getSum failed, getSum passed) of
    (0, 0, _) -> putStrLn $ colorise green "All tests passed"
    (n, f, p) ->
      putStrLn $
        unwords $
          filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where
    nPassed 0 = ""
    nPassed p = colorise green $ show p ++ " tests passed"
    nFailed 0 = ""
    nFailed f = colorise red $ show f ++ " tests failed"
    nNotImpl 0 = ""
    nNotImpl n = colorise yellow $ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
