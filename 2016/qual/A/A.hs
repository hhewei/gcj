module A ( main, test ) where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_)

import Data.IntSet (IntSet, empty, union, fromList, singleton)
import Data.List (find)

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "A-sample.txt" ReadMode run

------------------------------------------------------

solve :: Integer -> IO String
solve 0 = return "INSOMNIA"
solve n = do
  let s = series n
      f bits cur = bits `union` (digits cur)
      d = scanl f empty s
      Just (_,ans) = find (\(a,_) -> a == allDigits) $ zip (tail d) s
  return$ show ans

series :: Integer -> [Integer]
series n = map (n*) [1..]

digits :: Integer -> IntSet
digits 0 = singleton 0
digits x = digits' x where
  digits' 0 = empty
  digits' n =
    let d = n `div` 10
        r = n `mod` 10 in
    (digit r) `union` digits' d

digit :: Integer -> IntSet
digit r | r < 10 = singleton$ fromEnum r
digit _ = undefined

allDigits :: IntSet
allDigits = fromList [0..9]

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    n <- hReadLn h
    ans <- solve n
    printf "Case #%d: %s\n" (i :: Int) ans
