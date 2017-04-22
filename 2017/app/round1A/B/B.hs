
module Main ( main ) where

import Text.Printf (printf)
import Control.Monad (forM_, replicateM)

import Data.List (sort)
import Data.Ratio

readMany :: Read a => IO [a]
readMany = do
  l <- getLine
  return (map read (words l))

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n, p] <- readMany
    ingres <- readMany
    matrix <- replicateM n readMany
    let ans = solve ingres (map sort matrix)
    -- print ingres
    -- print matrix
    printf "Case #%d: %d\n" (i :: Int) ans

solve :: [Int] -> [[Int]] -> Int
solve reqs m = go 0 (f `map` zip reqs m)
  where
    f (r, as) = map (range r) as

    go :: Int -> [[(Int, Int)]] -> Int
    go p r | any ([]==) r = p
    go p r =
      let heads = map head r
          lower = maximum (map fst heads)
          upper = minimum (map snd heads) in
      if lower <= upper
        then
          go (p+1) (map tail r)
        else
          go p (map (dropWhile ((<lower) . snd)) r)

range :: Int -> Int -> (Int, Int)
range unit amt =
  let
    lower = (10*amt) % (11*unit)
    upper = (10*amt) % (9*unit)
  in (ceiling lower, floor upper)
