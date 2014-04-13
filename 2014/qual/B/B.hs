{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
import Data.String.Interpolate
import Control.Monad

solveCase :: Double -> Double -> Double -> Double
solveCase c f x = optimize 0 0 (x/2)
  where
    optimize k base total =
      let base' = base + c / (2 + k*f)
          total' = base' + x / (2 + (k+1)*f) in
      if total <= total' then
        total
      else
        optimize (k+1) base' total'

main :: IO ()
main = do
  (n :: Int) <- read `fmap` getLine
  forM_ [1..n] $ \k -> do
    [c, f, x] <- (map read . words) `fmap` getLine
    putStrLn [i|Case ##{k}: #{solveCase c f x}|]
