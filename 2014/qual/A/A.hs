{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
import Data.List (intersect)
import Data.String.Interpolate
import Control.Monad

newtype Grid = Grid [[Int]]
row :: Grid -> Int -> [Int]
row (Grid rows) n = rows !! (n - 1)

readGrid :: IO Grid
readGrid = Grid `fmap` replicateM 4 readRow
  where
    readRow = do
      ns <- words `fmap` getLine
      return $ map read ns

solveCase :: Int -> Grid -> Int -> Grid -> String
solveCase a1 g1 a2 g2 =
  let s1 = g1 `row` a1
      s2 = g2 `row` a2 in
  case s1 `intersect` s2 of
    [n] -> show n
    [] -> "Volunteer cheated!"
    _ -> "Bad magician!"

main :: IO ()
main = do
  (n :: Int) <- read `fmap` getLine
  forM_ [1..n] $ \k -> do
    a1 <- read `fmap` getLine
    g1 <- readGrid
    a2 <- read `fmap` getLine
    g2 <- readGrid
    putStrLn [i|Case ##{k}: #{solveCase a1 g1 a2 g2}|]
