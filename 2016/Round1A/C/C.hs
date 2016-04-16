module C where

import Control.Monad (forM_, forM)
import Control.Monad.Trans.State.Strict (evalState, get, put, State)
import Text.Printf (printf)
import Data.Maybe (catMaybes)
import Data.List (groupBy, sort)
import Data.IntSet as S (fromList, IntSet, empty, member, insert, elems)
import Data.IntMap.Strict as M (fromList, IntMap, size, lookup)

type Graph = IntMap IntSet

solve :: [Int] -> IO Int
solve bffs = do
  let fwd = zip [1..] bffs
      bwd = zip bffs [1..]
      dg = M.fromList fwd
      rg = buildG bwd
      g = buildG$ fwd ++ bwd
      ccs = divide g
      (cycles, chains) = unzip$ map (analyze dg rg) ccs
      ans = max (maximum cycles) (sum chains)
  -- print g
  -- print ccs
  return ans

buildG :: [(Int,Int)] -> Graph
buildG edges =
  let gs = groupBy (\e1 e2 -> fst e1 == fst e2) $ sort edges
      gs' = map (\g -> (fst$ head g, S.fromList$ map snd g)) gs in
  M.fromList gs'

divide :: Graph -> [IntSet]
divide g =
  let n = size g
      s = forM [1..n] $ \i -> do
        r <- dfs g i
        return$ case r of
          [] -> Nothing
          _ -> Just$ S.fromList r
      ccs = evalState s S.empty
  in catMaybes ccs

dfs :: Graph -> Int -> State IntSet [Int]
dfs g i = do
  v <- get
  if i `S.member` v
    then return []
    else do
    put (i `insert` v)
    let Just neighbours = M.lookup i g
    reacheable <- mapM (dfs g) $ elems neighbours
    return$ i : concat reacheable

findCycle :: IntMap Int -> IntSet -> [Int]
findCycle dg ns =
  let e = head$ elems ns in
  evalState (iter e) S.empty
  where
    iter e = iter' e []
    iter' cur path = do
      v <- get
      if cur `S.member` v
        then return$ cur : takeWhile (/= cur) path
        else do
        put (cur `insert` v)
        let Just next = M.lookup cur dg
        iter' next (cur:path)

longestChain :: Graph -> Int -> Int -> Int
longestChain _ avoid cur | cur == avoid = 0
longestChain g avoid cur =
  case M.lookup cur g of
    Just nexts -> 1 + maximum (map (longestChain g avoid) (elems nexts))
    _ -> 1

analyze :: IntMap Int -> Graph -> IntSet -> (Int, Int)
analyze dg rg ns =
  let cyc = findCycle dg ns
      len = length cyc
      chain = case cyc of
        [a, b] ->
          let chaina = longestChain rg a b
              chainb = longestChain rg b a in
          chaina + chainb
        _ -> 0
  in (len, chain)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    _ <- getLine
    ws <- words `fmap` getLine
    let bffs = map read ws
    ans <- solve bffs
    printf "Case #%d: %d\n" (i::Int) ans
