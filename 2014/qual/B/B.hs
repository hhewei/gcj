{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate

solve :: Double -> Double -> Double -> Double
solve c f x = optimize 0 0 (x/2)
  where
    optimize k base total =
      let base' = base + c / (2 + k*f)
          total' = base' + x / (2 + (k+1)*f) in
      if total <= total' then
        total
      else
        optimize (k+1) base' total'

solveOneCase :: String -> Double
solveOneCase input =
  let ws = words input
      [c, f, x] = map read ws in
  solve c f x

solveCases :: Int -> [String] -> IO ()
solveCases _ [] = return ()
solveCases n (h:t) = do
  let solution = solveOneCase h
  putStrLn [i|Case ##{n}: #{solution}|]
  solveCases (n+1) t

main :: IO ()
main = do
  (_: rest) <- lines `fmap` getContents
  solveCases 1 rest
