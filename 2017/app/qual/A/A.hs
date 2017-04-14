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
test = withFile "A-sample.txt" ReadMode run

------------------------------------------------------

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    line <- hGetLine h
    let [s, k] = words line
        ans = solve s (read k)
    printf "Case #%d: %s\n" (i :: Int) ans

solve :: String -> Int -> String
solve s k = case solve_ k s (length s) 0 of
  Just n -> show n
  _ -> "IMPOSSIBLE"

solve_ :: Int -> String -> Int -> Int -> Maybe Int
solve_ _ [] _ n = Just n
solve_ k ('+':as) l n = solve_ k as (l-1) n
solve_ k ('-':as) l n | l < k = Nothing
solve_ k s@('-':as) l n =
  let (toFlip, rest) = splitAt k s
      flipped = (flip' toFlip) ++ rest in
  solve_ k (tail flipped) (l-1) (n+1)
solve_ _ _ _ _ = undefined

flip' :: String -> String
flip' [] = []
flip' (a:as) = a' : flip' as
  where
    a' = case a of
      '-' -> '+'
      '+' -> '-'
      _ -> undefined
