{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestChapter3 where

import Chapter3.Folds
import Chapter3.FunctionsAsParameters
import Chapter3.ParametricPolymorphism as P
import Data.Function (on)
import Test.QuickCheck.Function
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
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
    "Exercise 3-2"
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

arbitraryNumberList :: (Arbitrary a, Eq a) => Gen (a, [a])
arbitraryNumberList = oneof [randomNumberList, randomNumberList `suchThat` uncurry elem]
  where
    randomNumberList = (,) <$> arbitrary <*> arbitrary

testFilterNot :: TestTree
testFilterNot =
  testGroup
    "filterNot returns the correct result"
    [ testProperty "...for Booleans" $
        forAll
          (arbitrary :: Gen [Bool])
          (propFilterNot id),
      testProperty "...for even/odd Ints" $
        forAll
          (arbitrary :: Gen [Int])
          (propFilterNot even)
    ]

propFilterNot :: (a -> Bool) -> ([a] -> Bool)
propFilterNot cond = not . any cond . filterNot cond

testFilterGovOrgs :: TestTree
testFilterGovOrgs =
  testGroup
    "filterGovOrgs"
    [ testCase "Written using `isGovOrg` auxiliary function" $
        assertBool "" True, --TODO
      testFilterGovOrgsFunc
        (filterGovOrgs :: ([Client Int] -> [Client Int])),
      testCase "Using a \\case expression" $
        assertBool "" True, --TODO
      testFilterGovOrgsFunc
        (filterGovOrgs' :: ([Client Int] -> [Client Int]))
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
    "Exercise 3-3"
    [ testProduct,
      testMinimumClient,
      testAll,
      testMinimumBy
    ]

testProduct :: TestTree
testProduct =
  testGroup
    "product"
    [ testCase "Written using pattern matching" $
        assertBool "" True, --TODO
      testProductFunc product',
      testCase "Written as a fold" $
        assertBool "" True, --TODO
      testProductFunc product''
    ]

testProductFunc :: ([Int] -> Int) -> TestTree
testProductFunc prodfunc =
  testProperty "... returns the correct result" $
    forAll
      arbitrary
      (\ns -> product ns == prodfunc ns)

testMinimumClient :: TestTree
testMinimumClient =
  testGroup
    "minimumClient"
    [ testCase "Written using pattern matching" $
        assertBool "" True, --TODO
      testMinClientError minimumClient',
      testMinClientFunc
        (minimumClient' :: [Client Int] -> Client Int), --TODO use types other than Int
      testCase "Written as a fold" $
        assertBool "" True, --TODO
      testMinClientError minimumClient'',
      testMinClientFunc
        (minimumClient'' :: [Client Int] -> Client Int) --TODO use types other than Int
    ]

testMinClientError :: ([Client i] -> Client i) -> TestTree
testMinClientError minClientFunc =
  expectExceptions ["Prelude.fold", ""] $
    testCase "... errors on an empty list" $
      minClientFunc [] `seq` assertBool "" True

testMinClientFunc :: (Eq i, Arbitrary i, Show i) => ([Client i] -> Client i) -> TestTree
testMinClientFunc minClientFunc =
  testProperty "... returns the correct result" $
    forAll
      arbitraryMinClient
      propMinClient
  where
    propMinClient (c, cs) = paramClient c == minClientFunc (map paramClient cs)

arbitraryMinClient :: Arbitrary i => Gen (AnyParamClient i, [AnyParamClient i])
arbitraryMinClient = do
  c <- arbitrary
  larger <- listOf $ arbitrary `suchThat` ((<) `on` _nameLength) c
  largerOrEqual <- listOf $ arbitrary `suchThat` ((<=) `on` _nameLength) c
  cs <- shuffle $ c : larger
  pure (c, cs ++ largerOrEqual)

_nameLength :: AnyParamClient i -> Int
_nameLength (AnyParamClient Individual {P.person}) = length . firstName $ person
_nameLength other = length . clientName . paramClient $ other

testAll :: TestTree
testAll =
  testGroup
    "all"
    [ testCase "Written using pattern matching" $
        assertBool "" True, --TODO
      testAllFunc all',
      testCase "Written as a fold" $
        assertBool "" True, --TODO
      testAllFunc all''
    ]

testAllFunc :: ([Bool] -> Bool) -> TestTree
testAllFunc allfunc =
  testProperty "... returns the correct result" $
    forAll
      (arbitrary :: Gen [Bool])
      (\bs -> and bs == allfunc bs)

testMinimumBy :: TestTree
testMinimumBy =
  testGroup
    "minimumBy"
    [ testGroup "... when g = (\\x -> -x)" $
        map
          ($ pure $ wrapSpecifiedFunction ((\x -> - x) :: (Int -> Int)))
          [testMinimumByError, testMinimumByFunc],
      testGroup "... for arbitrary functions" $
        map
          ($ (arbitrary :: Gen (Fun Int Int))) --TODO use types other than Int
          [testMinimumByError, testMinimumByFunc]
    ]

testMinimumByError :: (Show a, Show b, Ord b) => Gen (Fun a b) -> TestTree
testMinimumByError minByFunc =
  expectFail $
    testProperty "... errors on an empty list" $
      forAll
        minByFunc
        propMinimumByError
  where
    propMinimumByError g = minimumBy (applyFun g) [] `seq` True

testMinimumByFunc :: forall a b. (Arbitrary a, Show a, Show b, Ord b) => Gen (Fun a b) -> TestTree
testMinimumByFunc minByFunc =
  testProperty "... returns the correct result for nonempty lists" $
    forAll
      (arbitraryMinByFuncList minByFunc)
      propMinimumByFunc
  where
    propMinimumByFunc (funG, lst) = g (minimumBy g lst) == minimum (map g lst)
      where
        g = applyFun funG
    arbitraryMinByFuncList :: Gen (Fun a b) -> Gen (Fun a b, [a])
    arbitraryMinByFuncList arbitraryG = do
      funG <- arbitraryG
      lst <- arbitrary
      pure (funG, getNonEmpty lst)
