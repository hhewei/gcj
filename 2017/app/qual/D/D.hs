module Main ( main, test ) where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_, replicateM)
import Data.Maybe (fromJust)
import Data.List (foldl', intersperse)
import Data.Map.Strict as M (Map, fromList, elems, update, (!), assocs, differenceWith, map, unionWith)
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Query.MaxFlow
import Data.Graph.Inductive.PatriciaTree

hReadLn :: Read a => Handle -> IO a
hReadLn h = do
  l <- hGetLine h
  return (read l)

main :: IO ()
main = run stdin

test :: IO ()
test = withFile "C-sample.txt" ReadMode run

------------------------------------------------------

type Grid = M.Map (Int, Int) Char
data Update = Update Char (Int, Int)

instance Show Update where
  show (Update c (i,j)) = c : " " ++ (show i) ++ " " ++ (show j)

run :: Handle -> IO ()
run h = do
  t <- hReadLn h
  forM_ [1..t] $ \i -> do
    line <- hGetLine h
    let [n, m] = words line
    updates <- replicateM (read m) $ do
      ops <- hGetLine h
      let [m, r, c] = words ops
      return$ Update (head m) ((read r), (read c))
    let ans = solve (read n) updates
    printf "Case #%d: %s\n" (i :: Int) ans

stage :: Int -> Grid
stage n = fromList [ ((i,j), '.') | i <- [1..n], j <- [1..n]]

apply :: Grid -> Update -> Grid
apply g (Update c (i,j)) = update f (i,j) g
  where
    f '.' = Just c
    f 'o' = Just 'o'
    f a | a == c = Just c
    f _ = Just 'o'

points :: Grid -> Int
points g = (sum . Prelude.map point) $ elems g
  where
    point 'o' = 2
    point '+' = 1
    point 'x' = 1
    point _ = 0

diff :: Grid -> Grid -> [Update]
diff o n =
  let f c1 c2 | c1 /= c2 = Just c2
      f c1 c2 = Nothing
      g ((i,j), c) = Update c (i,j) in
  (Prelude.map g . assocs) $ differenceWith f o n

split :: Grid -> (Grid, Grid)
split g = (M.map extractp g, M.map extractx g)
  where
    extractp 'o' = '+'
    extractp a = a
    extractx 'o' = 'x'
    extractx a = a

maxMatching :: [LNode Int] -> [LEdge Int] -> Node -> Node -> [(Int, Int)]
maxMatching nodes edges s t =
  let
      graph :: Gr Int Int
      graph = mkGraph nodes edges
      matching :: Gr Int (Int, Int)
      matching = maxFlowgraph graph s t
      ps = [(fromJust$lab graph u, fromJust$lab graph v) | (u, v, (f,c)) <- labEdges matching, u /= s, v /= t, f>0]
  in ps

extendp :: Int -> Grid -> Grid
extendp n g =
  let pies = [p | p <- [2..(2*n)], not$ any (=='+') [g!(i,p-i) | i <- [1..n], 1<=p-i, p-i<=n]]
      nas = [na | na <- [(1-n)..(n-1)], not$ any (== '+') [g!(i,i-na) | i <- [1..n], 1<=i-na, i-na<=n]]
      k = 2*n - 1
      as = zip [1..k] pies
      bs = zip [(k+1)..(2*k)] nas
      s = 0
      t = -1
      nodes = (s,s):(t,t):(as ++ bs)
      inputs = [(s, v, 1) | (v,_) <- as]
      outputs = [(v, t, 1) | (v,_) <- bs]
      relation = [(fst u, fst v, 1) | u <- as, v <- bs, related u v ]

      related (_,x) (_,y) | (x + y)`mod`2 /= 0 = False
      related (_,x) (_,y) | (x - y)`mod`2 /= 0 = False
      related (_,x) (_,y) | ((x + y)`div`2) < 1 = False
      related (_,x) (_,y) | ((x + y)`div`2) > n = False
      related (_,x) (_,y) | ((x - y)`div`2) < 1 = False
      related (_,x) (_,y) | ((x - y)`div`2) > n = False
      related _ _ = True

      edges = inputs ++ relation ++ outputs

      ps = maxMatching nodes edges s t

      ps' = Prelude.map conv ps
      conv (p,n) = ((p+n)`div`2, (p-n)`div`2)
      ops = Prelude.map (Update '+') ps' in
  foldl' apply g ops

extendx :: Int -> Grid -> Grid
extendx n g =
  let rs = [i | i <- [1..n], not$ any (=='x') [g!(i,j) | j <- [1..n]]]
      cs = [j | j <- [1..n], not$ any (=='x') [g!(i,j) | i <- [1..n]]]
      ps = rs `zip` cs
      ops = Prelude.map (Update 'x') ps in
  foldl' apply g ops

merge :: Grid -> Grid -> Grid
merge ps xs = unionWith f ps xs
  where
    f '.' a = a
    f a '.' = a
    f a b | a == b = a
    f a b = 'o'

solve :: Int -> [Update] -> String
solve n ops =
  let e = stage n
      f = foldl' apply e ops
      (g, h) = split f
      g' = extendp n g
      h' = extendx n h
      r = merge g' h'
      p = points r
      s = diff f r
      l = length s in
  (concat . intersperse "\n") $ (show p ++ " " ++ show l) : Prelude.map show s
