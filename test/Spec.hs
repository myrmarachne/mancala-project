import Test.Framework (defaultMain)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Test.Framework.Providers.QuickCheck2 (testProperty)


main :: IO ()
main = defaultMain [testSuite1, testSuite2]

testSuite1 :: Test
testSuite1 = testGroup "Testing testSuite1"
   [testCase "first" (1 + 2 @?= 3), testCase "second" (1 + 3 @?= 4),
    testProperty "testowaniequickchecka" True]

testSuite2 :: Test
testSuite2 = testGroup "Testing testSuite2"
  [testCase "first2" (1 + 2 @?= 3), testCase "second2" (1 + 3 @?= 4)]
