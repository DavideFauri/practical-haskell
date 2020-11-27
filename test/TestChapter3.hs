module TestChapter3 where

import Chapter3.FunctionsAsParameters
import Chapter3.ParametricPolymorphism as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestUtils

chapter3Tests :: TestTree
chapter3Tests =
  testGroup
    "Chapter 3"
    [ testFilters,
      testFolds
    ]

-- EXERCISE 3-2

testFilters :: TestTree
testFilters =
  testGroup
    "Filters"
    [ testFilterOnes,
      testFilterANumber,
      testFilterNot,
      testFilterGovOrgs
    ]

testFilterOnes :: TestTree
testFilterOnes =
  testProperty "filterOnes returns the correct result" $
    forAll
      (arbitrary :: Gen [Int]) --TODO use types other than Int
      propFilterOnes
  where
    propFilterOnes = all (1 ==) . filterOnes

testFilterANumber :: TestTree
testFilterANumber =
  testProperty "filterANumber returns the correct result" $
    forAll
      (arbitraryNumberList :: Gen (Int, [Int])) --TODO use types other than Int
      propFilterANumber
  where
    propFilterANumber (n, lst) = all (n ==) $ filterANumber n lst

arbitraryNumberList :: (Arbitrary a, Num a, Eq a) => Gen (a, [a])
arbitraryNumberList = oneof [randomNumberList, randomNumberList `suchThat` (\(n, l) -> n `elem` l)]
  where
    randomNumberList = (,) <$> arbitrary <*> arbitrary

testFilterNot :: TestTree
testFilterNot =
  testGroup
    "filterNot returns the correct result"
    [ testProperty "...for Booleans" $
        forAll (arbitrary :: Gen [Bool]) (propFilterNot id),
      testProperty "...for even/odd Ints" $
        forAll (arbitrary :: Gen [Int]) (propFilterNot even)
    ]

propFilterNot :: (a -> Bool) -> ([a] -> Bool)
propFilterNot cond = not . any cond . filterNot cond

testFilterGovOrgs :: TestTree
testFilterGovOrgs =
  testGroup
    "filterGovOrgs"
    [ testCase "Written using `isGovOrg` auxiliary function" $ assertBool "" True, --TODO
      testFilterGovOrgsFunc (filterGovOrgs :: ([Client Int] -> [Client Int])),
      testCase "Using a \\case expression" $ assertBool "" True, --TODO
      testFilterGovOrgsFunc (filterGovOrgs' :: ([Client Int] -> [Client Int]))
    ]

testFilterGovOrgsFunc :: (Eq i, Show i, Arbitrary i) => ([Client i] -> [Client i]) -> TestTree
testFilterGovOrgsFunc filterFunc =
  testProperty "... returns the correct result" $
    forAll
      arbitraryGovOrgs
      propFilterGovOrgs
  where
    propFilterGovOrgs (gos, clients) = filterFunc (map paramClient clients) `setEquivalent` map paramClient gos
      where
        setEquivalent a b = all (`elem` b) a && all (`elem` a) b

arbitraryGovOrgs :: Arbitrary i => Gen ([AnyParamClient i], [AnyParamClient i])
arbitraryGovOrgs = do
  gos <- listOf (arbitrary `suchThat` (_isGovOrg . paramClient))
  not_gos <- listOf (arbitrary `suchThat` (not . _isGovOrg . paramClient))
  clients <- shuffle (gos ++ not_gos)
  pure (gos, clients)

_isGovOrg :: Client i -> Bool
_isGovOrg GovOrg {} = True
_isGovOrg _ = False

-- EXERCISE 3-3

testFolds :: TestTree
testFolds =
  testGroup
    "Folds"
    []
