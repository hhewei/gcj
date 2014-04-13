import Data.List (sort)

solveDeceitful :: [Double] -> [Double] -> Int
solveDeceitful naomi ken =
  let naomi' = sort naomi
      ken' = sort ken in
  solve naomi' ken'
  where
    solve [] [] = 0
    solve (n0:ns) k@(k0:ks) =
      if (n0 > k0) then
        1 + solve ns ks
      else
        solve ns (init k)
    solve _ _ = undefined

solveWar :: [Double] -> [Double] -> Int
solveWar naomi ken =
  let naomi' = reverse$ sort naomi
      ken' = reverse$ sort ken in
  solve naomi' ken'
  where
    solve [] [] = 0
    solve (n0:ns) k@(k0:ks) =
      if (n0 > k0) then
        1 + solve ns (init k)
      else
        solve ns ks
    solve _ _ = undefined

solveOneCase :: [String] -> (Int, Int)
solveOneCase input | length input == 3 =
  let naomi = map read $ words (input!!1)
      ken = map read $ words (input!!2)
      n = solveDeceitful naomi ken
      m = solveWar naomi ken in
  (n, m)
solveOneCase _ = undefined

solveCases :: Int -> [String] -> IO ()
solveCases _ [] = return ()
solveCases i input = do
  let (chunk, rest) = splitAt 3 input
      (n, m) = solveOneCase chunk
  putStr "Case #" >> (putStr$ show i) >> putStr ": "
  (putStr$ show n) >> putStr " " >> (putStrLn$ show m)
  solveCases (i+1) rest

main :: IO ()
main = do
  (_: rest) <- lines `fmap` getContents
  solveCases 1 rest

