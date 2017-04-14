module Main ( main, test ) where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_, replicateM)
import Data.List (foldl', intersperse)
import Data.Map.Strict (Map, fromList, elems, update, (!), assocs, differenceWith)

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "C-sample.txt" ReadMode run

------------------------------------------------------

type Grid = Map (Int, Int) Char
data Update = Update Char (Int, Int)

instance Show Update where
  show (Update c (i,j)) = c : " " ++ (show i) ++ " " ++ (show j)

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    line <- hGetLine h
    let [n, m] = words line
    updates <- replicateM (read m) $ do
      ops <- hGetLine h
      let [m, r, c] = words ops
      return$ Update (head m) ((read r), (read c))
    let ans = solve (read n) updates
    printf "Case #%d: %s\n" (i :: Int) ans

stage :: Int -> Grid
stage n = fromList [ ((i,j), '.') | i <- [1..n], j <- [1..n]]

apply :: Grid -> Update -> Grid
apply g (Update c (i,j)) = update f (i,j) g
  where
    f '.' = Just c
    f 'o' = Just 'o'
    f a | a == c = Just c
    f _ = Just 'o'

adjust :: Int -> Int -> Int
adjust n i | i > n = i`mod`n
adjust n i = i

points :: Grid -> Int
points g = (sum . map point) $ elems g
  where
    point 'o' = 2
    point '+' = 1
    point 'x' = 1
    point _ = 0

diff :: Grid -> Grid -> [Update]
diff o n =
  let f c1 c2 | c1 /= c2 = Just c2
      f c1 c2 = Nothing
      g ((i,j), c) = Update c (i,j) in
  (map g . assocs) $ differenceWith f o n

solve :: Int -> [Update] -> String
solve n ops =
  let e = stage n
      f = foldl' apply e ops
      x = head$ [j | j <- [1..n], f!(1,j) == 'x' || f!(1,j) == 'o'] ++ [1]
      g = foldl' apply f [Update 'x' (i, adjust n (i+x-1)) | i <- [1..n]]
      h = foldl' apply g [Update '+' (1,j) | j <- [1..n]]
      r = foldl' apply h [Update '+' (n,j) | j <- [2..(n-1)]]
      p = points r
      s = diff f r
      l = length s in
  (concat . intersperse "\n") $ (show p ++ " " ++ show l) : map show s
