{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
import Data.List (transpose)
import Control.Monad (forM_, msum)

f :: Int -> Int -> Int -> Maybe [String]
f 1 1 0 = Just ["c"]
f 1 c m = Just [ 'c' : [ if i <= c - m then '.' else '*' | i <- [2..c] ] ]

f 2 2 0 = Just [ "..", ".." ]
f 2 2 3 = Just [ ".*", "**" ]
f 2 2 _ = Nothing
f 2 c m | m `mod` 2 == 0 && 2*c - m >= 4 =
  let m' = m `div` 2 in
  Just . replicate 2 $ replicate (c - m') '.' ++ replicate m' '*'
        | m == 2 * c - 1 = Just ['.' : replicate (c - 1) '*', replicate c '*']
        | otherwise = Nothing

-- f r c m | r <= c = [ | r' <- [], c' <- []]
f r c m | r <= c = msum$ [ g r' c' | r' <- [1..r-1], c' <- [1..c-1], r'*c' + m >= r*c ]
                   ++ [ f r' c' m | r' <- [1..r-1], c' <- [1..c-1], (r - r') * (c - c') >= m ]
        | otherwise = transpose `fmap` f c r m
  where
    g r' c' = let m' = r'*c' + m - r*c in f r' c' m'

f' :: Int -> Int -> Int -> Maybe [String]
f' r c m
  | r <= 2 || c <= 2 = f r c m
  | r <= c = msum$ [ g r' c' | r' <- [2..r-1], c' <- [2..c-1], r'*c' + m >= r*c ]
             ++ [ f r' c' m | r' <- [2..r-1], c' <- [2..c-1], (r - r') * (c - c') >= m ]
  | otherwise = transpose `fmap` f' c r m
  where
    g r' c' = let m' = r'*c' + m - r*c in f r' c' m'

solveCase :: Int -> Int -> Int -> String
solveCase r c m = case f' r c m of
  Just s -> unlines s
  Nothing -> "Impossible\n"

p :: Int -> Int -> Int -> IO ()
p r c m = putStr$ solveCase r c m
  
main :: IO ()
main = do
  (n :: Int) <- read `fmap` getLine
  forM_ [1..n] $ \k -> do
    [r, c, m] <- words `fmap` getLine
    putStrLn$ "Case #" ++ show k ++ ":"
    putStr$ solveCase (read r) (read c) (read m)

