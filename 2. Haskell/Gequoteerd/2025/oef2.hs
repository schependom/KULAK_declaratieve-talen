-- 1.
vanDyck :: Int -> [String]
vanDyck 0 = [""]
vanDyck n =
  [ '(' : kleiner ++ ')' : rest
    | k <- [0 .. n - 1],
      kleiner <- vanDyck k, -- haakjes rond voorgaande van Dyck string van lengte 2k ...
      rest <- vanDyck (n - 1 - k) -- ... samengevoegd met van Dyck string van lengte 2(n - 1 - k) ...
      -- ... om zo een nieuwe string te krijgen van lengte 2 + 2k + 2(n-1-k) = 2n
  ]

vanDyck' :: Int -> [String]
vanDyck' n
  | n == 0 = [""]
  | otherwise =
      [ '(' : kleiner ++ ')' : rest
        | k <- [1 .. n],
          kleiner <- vanDyck' (n - k), -- lengte 2(n-k) = 2n - 2k
          rest <- vanDyck' (k - 1) -- lengte 2(k-1) = 2k - 2
          -- in totaal 2 (haakjes rond kleiner) + 2n - 2k + 2k - 2 = 2n
      ]

-- 2.
-- oneindige rij
allVanDyck :: [[String]]
allVanDyck = [vanDyck n | n <- [0 ..]]

-- 3.
aantalVanDyck :: Int -> Int
aantalVanDyck = length . vanDyck -- eta gereduceerd