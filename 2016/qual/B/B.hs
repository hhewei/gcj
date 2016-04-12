module B (main, test) where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_)

import Data.List (group)

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "B-sample.txt" ReadMode run

---------------------------------------------------

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    s <- hGetLine h
    let g = group s
        len = length g
        ans = if '+' == head (last g) then len - 1 else len
    printf "Case #%d: %d\n" (i :: Int) ans
