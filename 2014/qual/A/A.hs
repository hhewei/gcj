import Data.List (intersect)

newtype Grid = Grid [[Int]]

row :: Grid -> Int -> [Int]
row (Grid rows) i = (rows !! i)

readRow :: String -> [Int]
readRow line = let ns = words line in
  map read ns

readGrid :: [String] -> Grid
readGrid [r1, r2, r3, r4] = Grid$ map readRow [r1, r2, r3, r4]
readGrid _ = undefined

solveOneCase :: [String] -> String
solveOneCase input | length input == 10 =
  let a1 = read$ input!!0
      g1 = readGrid$ (input!!) `map` [1,2,3,4]
      a2 = read$ input!!5
      g2 = readGrid$ (input!!) `map` [6,7,8,9]
      s1 = g1 `row` (a1-1)
      s2 = g2 `row` (a2-1)
      r = s1 `intersect` s2 in
  case r of
    [n] -> show n
    [] -> "Volunteer cheated!"
    _ -> "Bad magician!"
      
solveOneCase _ = undefined

solveCases :: Int -> [String] -> IO ()
solveCases _ [] = return ()
solveCases i input = do
  let (chunk, rest) = splitAt 10 input
      solution = solveOneCase chunk
  putStr "Case #" >> (putStr $ show i) >> putStr ": "
  putStrLn solution
  solveCases (i+1) rest

main :: IO ()
main = do
  c <- getContents
  let (_ : rest) = lines c
  solveCases 1 rest

