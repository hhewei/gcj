{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe (Maybe, fromJust)
import Data.List (elemIndex)
import Control.Monad (forM_)
import Control.Exception (assert)
import Text.Printf (printf)

data IJK = Y | I | J | K
  deriving (Eq)
data Sign = P | N
  deriving (Eq)

data IJKNum = IJKNum {
	s :: Sign,
	n :: IJK
} deriving (Eq)

readijk :: Char -> IJKNum
readijk 'i' = IJKNum P I
readijk 'j' = IJKNum P J
readijk 'k' = IJKNum P K
readijk _ = undefined

neg :: Sign -> Sign
neg P = N
neg N = P

xor :: Sign -> Sign -> Sign
P `xor` P = P
P `xor` N = N
N `xor` P = N
N `xor` N = P

(#) :: IJKNum -> IJKNum -> IJKNum
(IJKNum s1 Y) # (IJKNum s2 n2) = IJKNum (s1 `xor` s2) n2
(IJKNum s1 n1) # (IJKNum s2 Y) = IJKNum (s1 `xor` s2) n1

(IJKNum s1 I) # (IJKNum s2 I) = IJKNum (neg$ s1 `xor` s2) Y
(IJKNum s1 J) # (IJKNum s2 J) = IJKNum (neg$ s1 `xor` s2) Y
(IJKNum s1 K) # (IJKNum s2 K) = IJKNum (neg$ s1 `xor` s2) Y

(IJKNum s1 I) # (IJKNum s2 J) = IJKNum (s1 `xor` s2) K
(IJKNum s1 J) # (IJKNum s2 K) = IJKNum (s1 `xor` s2) I
(IJKNum s1 K) # (IJKNum s2 I) = IJKNum (s1 `xor` s2) J

(IJKNum s1 K) # (IJKNum s2 J) = IJKNum (neg$ s1 `xor` s2) I
(IJKNum s1 J) # (IJKNum s2 I) = IJKNum (neg$ s1 `xor` s2) K
(IJKNum s1 I) # (IJKNum s2 K) = IJKNum (neg$ s1 `xor` s2) J

one :: IJKNum
one = IJKNum P Y

prd :: [IJKNum] -> IJKNum
prd ns = foldl (#) one ns

pow :: IJKNum -> Int -> IJKNum
pow n p
  | p < 4 = prd$ replicate p n
  | otherwise = pow n (p `mod` 4)

solve :: [IJKNum] -> Int -> String
solve nums rep 
  | product /= (IJKNum N Y) = "NO"
  | s == Nothing = "NO"
  | t == Nothing = "NO"
  | (fromJust s) + (fromJust t) >= rep * length nums = "NO"
  | otherwise = "YES"
  where
	product = pow (prd nums) rep
	s = elemIndex (IJKNum P I) . scanl (#) one . concat $ replicate (min 4 rep) nums
	t = elemIndex (IJKNum P K) . scanl (flip (#)) one . concat $ replicate (min 4 rep) (reverse nums)

main :: IO ()
main = do
  (t :: Int) <- readLn
  forM_ [1..t] $ \i -> do
    line <- getLine
    let (lstr, _:xstr) = break (==' ') line
        l = read lstr
        x = read xstr
    ijks <- (map readijk) `fmap` getLine
    let r = assert (l == length ijks) $ solve ijks x
    printf "Case #%d: %s\n" i r
