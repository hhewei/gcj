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
test = withFile "B-sample.txt" ReadMode run

------------------------------------------------------

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    s <- hGetLine h
    let ans = solve s
    printf "Case #%d: %s\n" (i :: Int) ans

solve :: String -> String
solve [] = undefined
solve "0" = []
solve [d] = [d]
solve s =
  let (p, r) = part s in
  case r of
    [] -> p
    _ ->
      let (h:t) = reverse p
          p' = reverse ((pred h):t) in
      (solve p') ++ (replicate (length r) '9')

part :: String -> (String, String)
part (a:as) = part_ [a] as
part _ = undefined

part_ :: String -> String -> (String, String)
part_ p [] = (reverse p, [])
part_ (p:ps) (h:rest) | h >= p = part_ (h:p:ps) rest
part_ p rest = (reverse p, rest)
