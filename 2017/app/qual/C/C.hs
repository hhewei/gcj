module Main ( main, test ) where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_)

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "C-sample.txt" ReadMode run

------------------------------------------------------

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    line <- hGetLine h
    let [n, k] = words line
        ans = solve (read n) (read k)
    printf "Case #%d: %s\n" (i :: Int) ans

log2 :: Integer -> Int
log2 1 = 0
log2 x = 1 + log2 (x `div` 2)

solve :: Integer -> Integer -> String
solve n k =
  let m = log2 k
      p = product $ replicate m 2
      g = (n - k) `div` p
      l = g `div` 2
      r = l + (g `mod` 2) in
  show r ++ " " ++ show l
