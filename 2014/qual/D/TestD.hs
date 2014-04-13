import D (solveDeceitful, solveWar)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)

sample1 :: Assertion
sample1 = do
  let naomi = [0.5]
      ken = [0.6]
  assertEqual "Sample 1 failed" 0 $ solveDeceitful naomi ken
  assertEqual "Sample 1 failed" 0 $ solveWar naomi ken

sample2 :: Assertion
sample2 = do
  let naomi = [0.7, 0.2]
      ken = [0.8, 0.3]
  assertEqual "Sample 2 failed" 1 $ solveDeceitful naomi ken
  assertEqual "Sample 2 failed" 0 $ solveWar naomi ken

sample3 :: Assertion
sample3 = do
  let naomi = [0.5, 0.1, 0.9]
      ken = [0.6, 0.4, 0.3]
  assertEqual "Sample 3 failed" 2 $ solveDeceitful naomi ken
  assertEqual "Sample 3 failed" 1 $ solveWar naomi ken

sample4 :: Assertion
sample4 = do
  let naomi = [0.186, 0.389, 0.907, 0.832, 0.959, 0.557, 0.300, 0.992, 0.899]
      ken = [0.916, 0.728, 0.271, 0.520, 0.700, 0.521, 0.215, 0.341, 0.458]
  assertEqual "Sample 4 failed" 8 $ solveDeceitful naomi ken
  assertEqual "Sample 4 failed" 4 $ solveWar naomi ken


main :: IO ()
main = defaultMain [
  testCase "sample1" sample1,
  testCase "sample2" sample2,
  testCase "sample3" sample3,
  testCase "sample4" sample4
  ]
