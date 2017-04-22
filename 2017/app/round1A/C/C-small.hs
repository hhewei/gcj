module Main where

import Control.Monad (forM_)
import Text.Printf (printf)
import Data.Sequence as Q (Seq(..), singleton, null, viewl, ViewL(..), (><), fromList)
import Data.Set as S (Set, empty, notMember, fromList, union)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  nCase <- readLn
  forM_ [1..nCase] $ \i -> do
    line <- getLine
    let [hd, ad, hk, ak, b, d] = words line
        ans = solve (read hd) (read ad) (read hk) (read ak) (read b) (read d)
    printf "Case #%d: %s\n" (i :: Int) ans

newtype State = State {
  components :: (Int, Int, Int, Int)
} deriving (Eq, Ord, Show)

attack :: State -> Maybe State
attack (State (h1, h2, a1, a2)) = validate$
  if a1 >= h2
    then State (h1, 0, a1, a2)
    else State (h1 - a2, h2 - a1, a1, a2)

buff, debuff, cure :: Int -> State -> Maybe State
buff b (State (h1, h2, a1, a2)) = validate$ State (h1 - a2, h2, a1 + b, a2)
debuff d (State (h1, h2, a1, a2)) =
  let a2' = max 0 (a2 - d) in
  validate$ State (h1 - a2', h2, a1, a2')
cure h (State (h1, h2, a1, a2)) = validate$ State (h - a2, h2, a1, a2)

validate :: State -> Maybe State
validate (State (h1, h2, a1, a2)) | h1 <= 0 = Nothing
validate s = Just$ s

solve :: Int -> Int -> Int -> Int -> Int -> Int -> String
solve hd ad hk ak b d = go (singleton (State (hd, hk, ad, ak), 0)) empty where
  go :: Seq (State, Int) -> Set State -> String
  go q v =
    case viewl q of
      EmptyL -> "IMPOSSIBLE"
      (cur,n) :< q' ->
        let
          State (_, h2, _, _) = cur
          nexts = filter (`notMember` v) $ catMaybes [
            attack cur,
            buff b cur,
            debuff d cur,
            cure hd cur ]
          new = Q.fromList$ zip nexts (repeat (n+1))
          v' = v `union` S.fromList nexts in
        if h2 == 0
          then (show n)
          else go (q' >< new) v'
