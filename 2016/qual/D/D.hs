import Control.Monad (forM_)
import Text.Printf (printf)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a, _, _] <- words `fmap` getLine
    let k = read a
    printf "Case #%d:" (i :: Int)
    forM_ [1..k] $ \p -> do
      printf " %d" (p :: Int)
    putStrLn ""
