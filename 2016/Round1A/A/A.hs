module A where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_)
import Data.List (foldl')

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "A-sample.txt" ReadMode run

---------------------------------------------------

solve :: String -> String
solve s = foldl' f [] s where
  f p c =
    let a = p ++ [c]
        b = c : p in
    max a b
    
run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    s <- hGetLine h
    printf "Case #%d: %s\n" (i :: Int) $ solve s
  return ()
