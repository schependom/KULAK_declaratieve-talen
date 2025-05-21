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

-- 2.
-- oneindige rij
allVanDyck :: [[String]]
allVanDyck = [vanDyck n | n <- [0 ..]]

-- 3.
aantalVanDyck :: Int -> Int
aantalVanDyck = length . vanDyck -- eta gereduceerd