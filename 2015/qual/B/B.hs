{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)
import Control.Monad (forM_)
import Control.Exception (assert)
import Text.Printf (printf)


solve :: [Int] -> Int
solve [] = 0
solve (m:rest)
  | m <= 3 = m
  | otherwise =
    let (x, y) = (m`div`2 + m`mod`2, m`div`2)
        ps = reverse$ sort (x:y:rest) in
    min m (1 + solve ps)

main :: IO ()
main = do
  (t :: Int) <- readLn
  forM_ [1..t] $ \i -> do
    d <- readLn
    ps <- (reverse . sort . map read . words) `fmap` getLine
    let r = assert (d == length ps) $ solve ps
    printf "Case #%d: %d\n" i r
