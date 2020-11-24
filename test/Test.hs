import Test.Tasty
import Test.Tasty.HUnit
import TestChapter2 (chapter2Tests)
import TestChapter3 (chapter3Tests)

main :: IO ()
main = defaultMain testChapters

testChapters :: TestTree
testChapters =
  testGroup
    "Practical Haskell"
    [ areTestsRunning,
      chapter2Tests,
      chapter3Tests
    ]

areTestsRunning :: TestTree
areTestsRunning = testCase "Tasty is running" $ assertBool "" True
