import Control.Monad (forM_)
import Text.Printf (printf)

solve :: Integer -> Integer -> Integer -> [Integer]
solve k c s | c * s < k = []
solve k c _ =
  recur k c [1..k]

recur :: Integer -> Integer -> [Integer] -> [Integer]
recur k c a =
  let (pre, post) = splitAt (fromEnum c) a in
  if length pre == 0
  then []
  else (num k pre) : recur k c post

num :: Integer -> [Integer] -> Integer
num k s = succ (num' 0 (map pred s)) where
  num' :: Integer -> [Integer] -> Integer
  num' p [] = p
  num' p (a:as) =
    num' (p*k + a) as
  
main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [s1, s2, s3] <- words `fmap` getLine
    let (k, c, s) = (read s1, read s2, read s3)
    printf "Case #%d:" (i :: Int)
    let sol = solve k c s
    if length sol == 0
      then printf " IMPOSSIBLE"
      else do
      forM_ sol $ \p -> do
        printf " %s" (show p)
    putStrLn ""
