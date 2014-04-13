{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module D (
  solveDeceitful,
  solveWar,
  main
  ) where

import Data.List (sort)
import Data.String.Interpolate
import Control.Monad

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

main :: IO ()
main = do
  (n :: Int) <- read `fmap` getLine
  forM_ [1..n] $ \k -> do
    _ <- getLine
    naomi <- (map read . words) `fmap` getLine
    ken <- (map read . words) `fmap` getLine
    let d = solveDeceitful naomi ken
        w = solveWar naomi ken 
    putStrLn [i|Case ##{k}: #{d} #{w}|]
