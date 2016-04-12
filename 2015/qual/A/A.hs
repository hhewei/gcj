{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad (forM_)
import Control.Exception (assert)
import Text.Printf (printf)

solve :: [(Int, Int)] -> Int
solve s = loop s 0 0 where
  loop [] _ cost = cost
  loop ((l,n):s) level cost
  	| level >= l = loop s (level + n) cost
  	| otherwise = let extra = l - level in
  		loop s (level + n + extra) (cost + extra)

main :: IO ()
main = do
  (t :: Int) <- readLn
  forM_ [1..t] $ \i -> do
  	line <- getLine
  	let (mstr, _:sstr) = break (' '==) line
  	    m = read mstr
  	    s = map (read . (:[])) sstr
  	    r = assert (m + 1 == length s) $ solve $ (zip [0..m] s)
  	printf "Case #%d: %d\n" i r
