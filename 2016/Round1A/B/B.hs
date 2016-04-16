module B where

import Text.Printf (printf)
import Control.Monad (forM_, replicateM)
import Data.List (intersperse)

import Data.List (group, sort)

solve :: [Int] -> [Int]
solve ns =
  let gs = group$ sort ns
      s = map (\g -> (head g, length g)) gs
      os = filter (\(_,c) -> 1 == (c `mod` 2)) s
      hs = map fst os in
  sort hs

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    ls <- replicateM (2*n-1) $ do
      ws <- words `fmap` getLine
      return$ map read ws
    let numbers = concat ls
        sol = solve numbers
    printf "Case #%d: " (i :: Int)
    putStrLn . concat . intersperse " " $ map show sol
    
