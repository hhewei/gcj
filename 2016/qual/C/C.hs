import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (forM_, mapM_)
import Text.Printf (printf)

series :: Int -> [Int]
series n = [ n + 2*d | d <- [0..] ]

solve :: Int -> Maybe (Int, [Integer])
solve n =
  let divisors = [ divisor x | b <- [2..10], let x = interprete b n ] in
  if all isJust divisors
  then Just (n, catMaybes divisors)
  else Nothing

interprete :: Int -> Int -> Integer
interprete base x =
  let f p c = p * (fromIntegral base) + (fromIntegral $ fromEnum c) in
  foldl f 0 (bits x)

bits :: Int -> [Bool]
bits x = reverse$ bits' x where
  bits' 0 = []
  bits' n =
    let (q, r) = (n `div` 2, n `mod` 2) in
    (1==r) : bits' q

divisor :: Integer -> Maybe Integer
divisor n = listToMaybe [ cand | cand <- takeWhile (\x -> x*x <= n) primes, (n `mod` cand) == 0 ]

primes :: [Integer]
primes = take bound $ filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

bound :: Int
bound = 1000

display :: (Int, [Integer]) -> IO ()
display (n, ds) = do
  let bsStr = concat $ map (show . fromEnum) (bits n)
  printf "%s" bsStr
  forM_ ds $ \d -> do
    printf " %d" d
  putStrLn ""

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a, b] <- words `fmap` getLine
    let (n, j) = (read a, read b)
        s = series (1 + product (replicate (n-1) 2))
        candidates = map solve s
        solutions = catMaybes candidates
        output = take j solutions
    printf "Case #%d:\n" (i :: Int)
    forM_ output display
