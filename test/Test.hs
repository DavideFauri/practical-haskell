import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain testChapters

testChapters :: TestTree
testChapters =
  testGroup
    "Practical Haskell"
    [ areTestsRunning,
    ]

areTestsRunning :: TestTree
areTestsRunning = testCase "Tasty is running" $ assertBool "" True
