import Test.Tasty
import Test.Tasty.HUnit
import TestChapter2 (chapter2Tests)

main :: IO ()
main = defaultMain testChapters

testChapters :: TestTree
testChapters =
  testGroup
    "Practical Haskell"
    [ areTestsRunning,
      chapter2Tests
    ]

areTestsRunning :: TestTree
areTestsRunning = testCase "Tasty is running" $ assertBool "" True
