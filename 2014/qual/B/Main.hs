solve :: Double -> Double -> Double -> Double
solve c f x = optimize 0 0 (x/2)
  where
    optimize i base total =
      let base' = base + c / (2 + i*f)
          total' = base' + x / (2 + (i+1)*f) in
      if total <= total' then
        total
      else
        optimize (i+1) base' total'

solveOneCase :: String -> Double
solveOneCase input =
  let ws = words input
      [c, f, x] = map read ws in
  solve c f x

solveCases :: Int -> [String] -> IO ()
solveCases _ [] = return ()
solveCases i (h:t) = do
  let solution = solveOneCase h
  putStr "Case #" >> (putStr$ show i) >> putStr ": "
  print solution
  solveCases (i+1) t

main :: IO ()
main = do
  (_: rest) <- lines `fmap` getContents
  solveCases 1 rest
