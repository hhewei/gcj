import Control.Monad (forM_)

main = do
  let s = [0..1000000]
  print (length s)
  forM_ s print
